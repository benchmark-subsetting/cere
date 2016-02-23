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
// This file implements the interface to tear out a parallel region
// into a new function, replacing it with a call to the new function.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "region-outliner"
#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/LoopPass.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Transforms/Scalar.h"
#include <sstream>
#include <fstream>
#include <iostream>
#include <string>
#include "RegionExtractor.h"

#if LLVM_VERSION_MINOR == 5
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/DebugInfo.h"
#else
#include "llvm/DebugInfo.h"
#include "llvm/Support/InstIterator.h"
#endif

using namespace llvm;

STATISTIC(NumExtracted, "Number of regions extracted");

extern cl::opt<std::string> IsolateRegion;
extern cl::opt<bool> AppMeasure;
extern cl::opt<bool> PcereUse;

namespace {
struct OmpRegionOutliner : public FunctionPass {
  static char ID; // Pass identification, replacement for typeid
  unsigned NumLoops;
  bool ProfileApp;
  bool Pcere;
  std::string RegionName;

  explicit OmpRegionOutliner(unsigned numLoops = ~0,
                             const std::string &regionName = "")
      : FunctionPass(ID), NumLoops(numLoops), ProfileApp(AppMeasure),
        Pcere(PcereUse), RegionName(regionName) {
    if (regionName.empty())
      RegionName = IsolateRegion;
  }

  virtual bool runOnFunction(Function &F);

  virtual void getAnalysisUsage(AnalysisUsage &AU) const {
    AU.addRequiredID(BreakCriticalEdgesID);
    AU.addRequired<LoopInfo>();
    AU.addPreserved<LoopInfo>();
#if LLVM_VERSION_MINOR == 5
    AU.addRequired<DominatorTreeWrapperPass>();
    AU.addPreserved<DominatorTreeWrapperPass>();
#else
    AU.addRequired<DominatorTree>();
    AU.addPreserved<DominatorTree>();
#endif
  }
};
}

char OmpRegionOutliner::ID = 0;
static RegisterPass<OmpRegionOutliner>
    X("omp-region-outliner", "Outline parallel regions", false, false);

/// \brief Removes extension from \p filename.
std::string removeExtension(const std::string &filename) {
  size_t lastdot = filename.find_last_of(".");
  if (lastdot == std::string::npos)
    return filename;
  return filename.substr(0, lastdot);
}

/// \brief Removes character \p toReplace by \p replacer in \p str.
std::string removeChar(std::string str, const char toReplace,
                       const char replacer) {
  replace(str.begin(), str.end(), toReplace, replacer);
  return str;
}

/// \brief Update file format.
std::string updateFileFormat(std::string str) {
  str = removeChar(str, '-', '_');
  str = removeChar(str, '/', '_');
  str = removeChar(str, '+', '_');
  str = removeChar(str, '.', '_');
  return str;
}

/// \brief Creates the CERE formated function name for the outlined region.
/// The syntax is __cere__filename__functionName__firstLine
std::string createFunctionName(Function *oldFunction, CallInst *callInst) {
  std::string Separator = "_";

  // Get current module
  Module *mod = oldFunction->getParent();
  std::string File = mod->getModuleIdentifier();
  std::string newFunctionName;
  std::ostringstream oss;
  // If the function containing the loop does not have debug
  // information, we can't outline the loop.
  if (MDNode *firstN = callInst->getMetadata("dbg")) {
    DILocation firstLoc(firstN);
    oss << firstLoc.getLineNumber();
    std::string firstLine = oss.str();
    std::string Original_location = firstLoc.getFilename().str();
    std::string path = firstLoc.getDirectory();
    newFunctionName = "__cere__" + removeExtension(File) + Separator +
                      oldFunction->getName().str() + Separator + firstLine;
    newFunctionName = updateFileFormat(newFunctionName);
  }
  // Create a parallel region name for others iterations
  else {
    newFunctionName = "__cere__" + removeExtension(File) + Separator +
                      oldFunction->getName().str() + Separator + "first";
    newFunctionName = updateFileFormat(newFunctionName);
  }
  return newFunctionName;
}

/// Specify if the call has to be extracted
bool ChooseExtract(std::string functionCall, std::string RegionName,
                   CallInst *callInst, Function *F) {

  if (RegionName == "all") {
    if (functionCall.find("__kmpc_fork_call") != std::string::npos)
      return true;
    else
      return false;
  } else {
    if (functionCall.find("__kmpc_fork_call") != std::string::npos &&
        RegionName.compare(createFunctionName(F, callInst)) == 0)
      return true;

    else
      return false;
  }
}

/// Extract in a new basic block the omp fork call
int SplitOMPCall(Module *mod, Function *F, Pass *P, std::string RegionName) {
  std::string functionCall;
  int t = 0;

  CallInst *iFirst;
  Instruction *iSecond;

  // XXX To optimize with a single run trough the instructions
  for (inst_iterator I = inst_begin(F), E = inst_end(F); I != E; ++I) {
    if (t) {
      Instruction *instru = dyn_cast<Instruction>(&*I);
      iSecond = instru;
      break;
    }
    if (CallInst *callInst = dyn_cast<CallInst>(&*I)) {
      if (callInst->getCalledFunction()) {
        functionCall = callInst->getCalledFunction()->getName();
        if (ChooseExtract(functionCall, RegionName, callInst, F)) {
          t = 1;
          iFirst = callInst;
        }
      }
    }
  }
  if (t) {
    SplitBlock(iFirst->getParent(), iFirst, P);
    SplitBlock(iSecond->getParent(), iSecond, P);
  }

  return t;
}

/// Extract in a new function the omp fork call basic block
bool ExtractOMPCall(Module *mod, Function *F, Pass *P, bool ProfileApp,
                    std::string RegionName) {
  std::string functionCall;
  for (inst_iterator I = inst_begin(F), E = inst_end(F); I != E; ++I) {
    if (CallInst *callInst = dyn_cast<CallInst>(&*I)) {
      if (callInst->getCalledFunction()) {
        functionCall = callInst->getCalledFunction()->getName();
        if (ChooseExtract(functionCall, RegionName, callInst, F)) {
          RegionExtractor Extractor = RegionExtractor(
              callInst->getParent(), RegionName, ProfileApp, true);
          Extractor.extractCodeRegion();
          break;
        }
      }
    }
  }
  return true;
}

bool OmpRegionOutliner::runOnFunction(Function &F) {
  Module *mod = F.getParent();

  if (F.getName().find("__cere__") != std::string::npos)
    return false;
  if (F.getName().find(".omp_microtask") != std::string::npos)
    return false;
  int work = 1;
  while (work) {
    work = SplitOMPCall(mod, &F, this, RegionName);
    ExtractOMPCall(mod, &F, this, ProfileApp, RegionName);
  }

  return true;
}
