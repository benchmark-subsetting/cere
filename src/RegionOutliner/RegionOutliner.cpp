//===- RegionOutliner.cpp - Extract each loop into a new function ----------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under ... License. See LICENSE.TXT for details.
//
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

using namespace llvm;

STATISTIC(NumExtracted, "Number of regions extracted");

static cl::opt<std::string>
IsolateRegion("isolate-region", cl::init("all"),
                  cl::value_desc("String"),
                  cl::desc("RegionOutliner will only isolate this region"));
static cl::opt<bool>
AppMeasure("instrument-app", cl::init(false),
                  cl::value_desc("Boolean"),
                  cl::desc("If you want to isolate regions to profile the application"));

namespace {
    struct LoopExtractorAll : public LoopPass {
        static char ID; // Pass identification, replacement for typeid
        unsigned NumLoops;
        bool ProfileApp;
        std::string RegionName;

        explicit LoopExtractorAll(unsigned numLoops = ~0, const std::string &regionName = "") 
        : LoopPass(ID), NumLoops(numLoops), ProfileApp(AppMeasure), RegionName(regionName) {
            if (regionName.empty()) RegionName = IsolateRegion;
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
static RegisterPass<LoopExtractorAll> X("region-outliner", "Outline all loops", false, false);

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
    // If this loop is inside a function prefixed with __cere__
    // it means we are looking at an already outlined loop
    bool ShouldExtractLoop = true;
    Function* function = L->getHeader()->getParent();
    std::string name = function->getName();
    std::size_t found = name.find("__cere__");
    if (found != std::string::npos) {
        ShouldExtractLoop = false;
    }

    if (ShouldExtractLoop) {
        if (NumLoops == 0) return Changed;
        --NumLoops;
        CodeExtractorAll Extractor(DT, *L, RegionName, ProfileApp);
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
