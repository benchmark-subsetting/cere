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
// This file insert an init function at the beginning of programm. The close
// function is registered through atexit. It also inserts start and stop probes
// around a requested region. These functions can be linked with any user
// library.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "region-instrumentation"
#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/LoopPass.h"
#include <llvm/IR/Instructions.h>
#include "llvm/Transforms/Scalar.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include <fstream>
#include <sstream>
#include <errno.h>
#include "RegionInstrumentation.h"

#undef LLVM_BINDIR
#include "config.h"
#if LLVM_VERSION_MINOR == 5
#include "llvm/IR/Dominators.h"
#include "llvm/IR/DebugInfo.h"
#else
#include "llvm/DebugInfo.h"
#endif

using namespace llvm;
enum {
  VIVO,
  VITRO
};

STATISTIC(LoopCounter, "Counts number of regions instrumented");

extern cl::opt<std::string> IsolateRegion;
extern cl::opt<std::string> LoopsFilename;
extern cl::opt<bool> AppMeasure;
extern cl::opt<bool> Replay;
extern cl::opt<int> Invocation;

namespace {
struct LoopRegionInstrumentation : public FunctionPass {
  static char ID;
  unsigned NumLoops;
  std::string RegionName;
  std::string Separator;
  std::string RegionFile;
  int Mode;
  int RequestedInvoc;
  bool ReadFromFile;
  bool MeasureAppli;
  std::vector<std::string> LoopsToInstrument;
  LoopInfo *LI; // The current loop information

  explicit LoopRegionInstrumentation(unsigned numLoops = ~0,
                                     const std::string &regionName = "")
      : FunctionPass(ID), NumLoops(numLoops), RegionName(regionName),
        Mode(VIVO), Separator("_"), RegionFile(LoopsFilename),
        RequestedInvoc(Invocation), MeasureAppli(AppMeasure) {
    if (Replay)
      Mode = VITRO;
    if (regionName.empty())
      RegionName = IsolateRegion;
    if (RegionFile.empty())
      ReadFromFile = false;
    else {
      // We read a file, we need to empty RegionName
      RegionName = "";
      // Open the file and add regions in LoopsToInstrument
      std::string line;
      std::ifstream loopstream;
      loopstream.open(RegionFile.c_str(), std::ios::in);
      if (loopstream.is_open()) {
        while (getline(loopstream, line)) {
          LoopsToInstrument.push_back(line);
        }
        loopstream.close();
        ReadFromFile = true;
      } else {
        errs() << "Can't open file > " << RegionFile.c_str() << " : "
               << strerror(errno) << "\n";
        ReadFromFile = false;
      }
    }
  }

  virtual bool runOnFunction(Function &F);
  std::string createFunctionName(Loop *L, Function *oldFunction);
  bool visitLoop(Loop *L, Module *mod);

  virtual void getAnalysisUsage(AnalysisUsage &AU) const {
    AU.addRequiredID(BreakCriticalEdgesID);
    AU.addRequired<LoopInfo>();
    AU.addPreserved<LoopInfo>();
#if LLVM_VERSION_MINOR == 5
    AU.addRequired<DominatorTreeWrapperPass>();
#else
    AU.addRequired<DominatorTree>();
#endif
  }
};
}

char LoopRegionInstrumentation::ID = 0;
static RegisterPass<LoopRegionInstrumentation>
X("region-instrumentation", "Instrument regions with probes", false, false);

/// \brief Creates the CERE formated function name for the outlined region.
/// The syntax is __cere__filename__functionName__firstLine
std::string
LoopRegionInstrumentation::createFunctionName(Loop *L, Function *oldFunction) {
  // Get current module
  Module *mod = oldFunction->getParent();
  std::string module_name = mod->getModuleIdentifier();

  std::string newFunctionName;
  std::ostringstream oss;
  BasicBlock *firstBB = L->getBlocks()[0];

  if (MDNode *firstN = firstBB->front().getMetadata("dbg")) {
    DILocation firstLoc(firstN);
    oss << firstLoc.getLineNumber();
    std::string firstLine = oss.str();
    std::string Original_location = firstLoc.getFilename().str();
    std::string File = module_name;
    std::string path = firstLoc.getDirectory();

    newFunctionName = "__cere__" + removeExtension(File) + Separator +
                      oldFunction->getName().str() + Separator + firstLine;
    newFunctionName = updateFileFormat(newFunctionName);
  }

  return newFunctionName;
}

/// \brief This function find if the string \p newFunctionName is present in
/// the regions file
bool LoopRegionInstrumentation::runOnFunction(Function &F) {
  prepareInstrumentation(F, RegionFile, MeasureAppli, Mode, RequestedInvoc);

  Module *mod = F.getParent();
  // If we want to instrument a region, visit every loops.
  if (!MeasureAppli) {
    LoopInfo &LI = getAnalysis<LoopInfo>();
    std::vector<Loop *> SubLoops(LI.begin(), LI.end());
    for (unsigned i = 0, e = SubLoops.size(); i != e; ++i)
      visitLoop(SubLoops[i], mod);
  }
  return true;
}

/// \brief Runs on each loop and inserts probes around the requested ones
bool LoopRegionInstrumentation::visitLoop(Loop *L, Module *mod) {
  // Get current function
  Function *currFunc = L->getHeader()->getParent();

  // Ensure that we are working on outermost loops
  if (L->getLoopDepth() > 1)
    return false;
  if (!L->isLoopSimplifyForm()) {
    DEBUG(dbgs() << "Loop found, but not in simple form...\n");
    return false;
  }

  // If the loop is inside a CERE function it means it has already been
  // extracted. Otherwise just generate the name of the corresponding region
  std::string newFunctionName;
  std::size_t found = currFunc->getName().find("__cere__");
  if (found == std::string::npos)
    newFunctionName = createFunctionName(L, currFunc);
  else
    newFunctionName = currFunc->getName();
  if (newFunctionName.empty())
    return false;

  // If it's not the requested region, and we don't read regions from a file
  if (RegionName != "all" && RegionName != newFunctionName && !ReadFromFile) {
    return false;
  }
  // If we read regions to instrument from a file, check if the current region
  // is in that file
  else if (ReadFromFile) {
    bool found = false;
    for (std::vector<std::string>::iterator it = LoopsToInstrument.begin(),
                                            et = LoopsToInstrument.end();
         it != et; it++) {
      std::size_t pos = it->find(newFunctionName);
      if (pos != std::string::npos) {
        found = true;
        break;
      }
    }
    if (!found)
      return false;
  }

  ++LoopCounter;
  // Create start and stop functions signatures
  FunctionType *FuncTy = createFunctionType(mod);
  Function *func_start = createFunction(FuncTy, mod, "cere_markerStartRegion");
  Function *func_stop = createFunction(FuncTy, mod, "cere_markerStopRegion");

  // Get predecessor and exit basic blocks of the current loop
  BasicBlock *PredBB = L->getLoopPredecessor();
  SmallVector<BasicBlock *, 8> exitblocks;
  L->getExitBlocks(exitblocks);
  if (PredBB == NULL || exitblocks.size() == 0) {
    errs() << "Can't find Prdecessor or successor basick block for region "
           << RegionName << "\n";
    DEBUG(dbgs() << "Can't find predecessor or successor basick block for \
                    region " << RegionName << "\n");
    return false;
  }
  std::vector<Value *> funcParameter;

  // If requested invocation to measure is different than 0. It means we want
  // to record a particular invocation. We must then create a counter to count
  // the current invocation of the region
  if (RequestedInvoc != 0) {
    // Create invocation counter
    GlobalVariable *gvar_int32_count = create_invocation_counter(mod);

    // Load the invocation counter
    LoadInst *int32_0 =
        new LoadInst(gvar_int32_count, "", false, &PredBB->back());
    int32_0->setAlignment(4);
    // Increments it
    ConstantInt *const_int32_1 =
        ConstantInt::get(mod->getContext(), APInt(32, StringRef("1"), 10));
    BinaryOperator *int32_inc = BinaryOperator::Create(
        Instruction::Add, int32_0, const_int32_1, "inc", &PredBB->back());
    // Save the value
    StoreInst *void_0 =
        new StoreInst(int32_inc, gvar_int32_count, false, &PredBB->back());
    void_0->setAlignment(4);
    // Load the updated invocation counter
    LoadInst *int32_1 =
        new LoadInst(gvar_int32_count, "", false, &PredBB->back());
    int32_1->setAlignment(4);

    // Create function parameters
    funcParameter = createFunctionParameters(mod, newFunctionName, Mode,
                                             RequestedInvoc, int32_1);
  } else {
    // Create function parameters
    funcParameter =
        createFunctionParameters(mod, newFunctionName, Mode, RequestedInvoc);
  }

  // Add the call of cere start probe as the last instruction of the
  // predecessor basic block
  CallInst::Create(func_start, funcParameter, "", &PredBB->back());
  // Add the call of cere stop probe as the last instruction of all exit
  // basic block
  for (SmallVectorImpl<BasicBlock *>::iterator I = exitblocks.begin(),
                                               E = exitblocks.end();
       I != E; ++I) {
    CallInst::Create(func_stop, funcParameter, "", &((*I)->back()));
  }
  return true;
}
