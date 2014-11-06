//===- LoopExtractor.cpp - Extract each loop into a new function ----------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// A pass wrapper around the ExtractLoop() scalar transformation to extract each
// top-level loop into its own new function. If the loop is the ONLY loop in a
// given function, it is not touched. This is a pass most useful for debugging
// via bugpoint.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "loop-extract-all"
#include "llvm/Transforms/IPO.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/Dominators.h"
#include "llvm/Analysis/LoopPass.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/Pass.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "CodeExtractorAll.h"
#include <fstream>
#include <set>
using namespace llvm;

STATISTIC(NumExtracted, "Number of loops extracted");

static cl::opt<std::string>
IsolateLoop("isolate-loop", cl::init("all"),
                  cl::value_desc("loopname"),
                  cl::desc("loop-extract-all will only isolate this loop"));
static cl::opt<bool>
AppMeasure("instrument-app", cl::init(false),
                  cl::value_desc("Boolean"),
                  cl::desc("if True, application time will be measured"));

namespace {
  struct LoopExtractorAll : public LoopPass {
    static char ID; // Pass identification, replacement for typeid
    unsigned NumLoops;
    std::string Loopname;
    bool measureAppli;

    explicit LoopExtractorAll(unsigned numLoops = ~0, const std::string &loopname = "") 
      : LoopPass(ID), NumLoops(numLoops), Loopname(loopname), measureAppli(AppMeasure) {
        if (loopname.empty()) Loopname = IsolateLoop;
      }

    virtual bool runOnLoop(Loop *L, LPPassManager &LPM);

    virtual void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.addRequiredID(BreakCriticalEdgesID);
      AU.addRequiredID(LoopSimplifyID);
      AU.addRequired<DominatorTree>();
    }
  };
}

char LoopExtractorAll::ID = 0;
static RegisterPass<LoopExtractorAll> X("loop-extract-all", "Extract all loops", false, false);

bool LoopExtractorAll::runOnLoop(Loop *L, LPPassManager &LPM) {
  // Only visit top-level loops.
  if (L->getParentLoop())
    return false;

  // If LoopSimplify form is not available, stay out of trouble.
  if (!L->isLoopSimplifyForm())
    return false;

  DominatorTree &DT = getAnalysis<DominatorTree>();
  bool Changed = false;
  
  // Extract the loop if it was not previously extracted:
	// iterate through the kernelgen.extracted metadata nodes and
	// check whether the loop being extracted is already there.
	bool ShouldExtractLoop = true;
	Function* function = L->getHeader()->getParent();
	std::string name = function->getName();
	std::size_t found = name.find("__extracted__");
    if (found != std::string::npos) { //means we are looking at an already extracted loop
        ShouldExtractLoop = false;
    }

  if (ShouldExtractLoop) {
    if (NumLoops == 0) return Changed;
    --NumLoops;
    CodeExtractorAll Extractor(DT, *L, Loopname, measureAppli);
    if (Extractor.extractCodeRegion() != 0) {
      Changed = true;
      // After extraction, the loop is replaced by a function call, so
      // we shouldn't try to run any more loop passes on it.
      LPM.deleteLoopFromQueue(L);
    }
    ++NumExtracted;
  }

  return Changed;
}
