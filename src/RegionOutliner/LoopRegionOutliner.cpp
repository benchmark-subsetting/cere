/*****************************************************************************
 * This file is part of CERE.                                                *
 *                                                                           *
 * Copyright (c) 2013-2015, Universite de Versailles St-Quentin-en-Yvelines  *
 *                                                                           *
 * CERE is free software: you can redistribute it and/or modify it under     *
 * the terms of the GNU Lesser General Public License as published by        *
 * the Free Software Foundation, either version 3 of the License,            *
 * or (at your option) any later version.                                    *
 *                                                                           *
 * CERE is distributed in the hope that it will be useful,                   *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of            *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             *
 * GNU General Public License for more details.                              *
 *                                                                           *
 * You should have received a copy of the GNU General Public License         *
 * along with CERE.  If not, see <http://www.gnu.org/licenses/>.             *
 *****************************************************************************/

//===----------------------------------------------------------------------===//
//
// A pass wrapper around the ExtractLoop() scalar transformation to extract each
// top-level loop into its own new function.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "region-outliner"
#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/LoopPass.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Transforms/Scalar.h"
#include "RegionExtractor.h"
#include "llvm/IR/Dominators.h"

using namespace llvm;

STATISTIC(NumExtracted, "Number of regions extracted");

extern cl::opt<std::string> IsolateRegion;
extern cl::opt<bool> AppMeasure;
extern cl::opt<bool> PcereUse;

namespace {
struct LoopRegionOutliner : public LoopPass {
  static char ID; // Pass identification, replacement for typeid
  unsigned NumLoops;
  bool ProfileApp;
  bool Pcere;
  std::string RegionName;

  explicit LoopRegionOutliner(unsigned numLoops = ~0,
                          const std::string &regionName = "")
      : LoopPass(ID), NumLoops(numLoops), ProfileApp(AppMeasure),
        Pcere(PcereUse), RegionName(regionName) {
    if (regionName.empty())
      RegionName = IsolateRegion;
  }

  virtual bool runOnLoop(Loop *L, LPPassManager &LPM);

    void getAnalysisUsage(AnalysisUsage &AU) const override {
      AU.addRequiredID(BreakCriticalEdgesID);
      AU.addRequiredID(LoopSimplifyID);
      AU.addRequired<DominatorTreeWrapperPass>();
      AU.addRequired<LoopInfoWrapperPass>();
    }
};
}

char LoopRegionOutliner::ID = 0;
static RegisterPass<LoopRegionOutliner> X("region-outliner", "Outline all loops",
                                      false, false);

bool LoopRegionOutliner::runOnLoop(Loop *L, LPPassManager &LPM) {
  // Only visit top-level loops.
  if (L->getParentLoop())
    return false;

  // If LoopSimplify form is not available, stay out of trouble.
  if (!L->isLoopSimplifyForm())
    return false;
  DominatorTree &DT = getAnalysis<DominatorTreeWrapperPass>().getDomTree();
  LoopInfo &LI = getAnalysis<LoopInfoWrapperPass>().getLoopInfo();
  bool Changed = false;

  // Extract the loop if it was not previously extracted:
  // If this loop is inside a function prefixed with __cere__
  // it means we are looking at an already outlined loop
  bool ShouldExtractLoop = true;
  Function *function = L->getHeader()->getParent();
  std::string name = function->getName();
  std::size_t found = name.find("__cere__");
  if (found != std::string::npos) {
    ShouldExtractLoop = false;
  }

  // Extract the loop if the entry block doesn't branch to the loop header.
  TerminatorInst *EntryTI =
    L->getHeader()->getParent()->getEntryBlock().getTerminator();
  if (!isa<BranchInst>(EntryTI) ||
      !cast<BranchInst>(EntryTI)->isUnconditional() ||
      EntryTI->getSuccessor(0) != L->getHeader()) {
    ShouldExtractLoop = true;
  } else {
    // Check to see if any exits from the loop are more than just return
    // blocks.
    SmallVector<BasicBlock*, 8> ExitBlocks;
    L->getExitBlocks(ExitBlocks);
    for (unsigned i = 0, e = ExitBlocks.size(); i != e; ++i)
      if (!isa<ReturnInst>(ExitBlocks[i]->getTerminator())) {
        ShouldExtractLoop = true;
        break;
      }
  }

  if (ShouldExtractLoop) {
  // We must omit EH pads. EH pads must accompany the invoke
  // instruction. But this would result in a loop in the extracted
  // function. An infinite cycle occurs when it tries to extract that loop as
  // well.
  SmallVector<BasicBlock*, 8> ExitBlocks;
  L->getExitBlocks(ExitBlocks);
  for (unsigned i = 0, e = ExitBlocks.size(); i != e; ++i)
    if (ExitBlocks[i]->isEHPad()) {
      ShouldExtractLoop = false;
      break;
    }
  }

  if (ShouldExtractLoop) {
    if (NumLoops == 0) return Changed;
    --NumLoops;
    RegionExtractor Extractor(DT, *L, "", ProfileApp);
    if (Extractor.extractCodeRegion() != nullptr) {
      Changed = true;
      // After extraction, the loop is replaced by a function call, so
      // we shouldn't try to run any more loop passes on it.
      LI.markAsRemoved(L);
    }
    ++NumExtracted;
  }

  return Changed;
}
