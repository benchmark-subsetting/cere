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
// library. Used for OpenMP regions.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "region-instrumentation"
#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/LoopPass.h"
#include <llvm/IR/Instructions.h>
#include "llvm/Transforms/Scalar.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/CommandLine.h"
#include <fstream>
#include <sstream>
#include <errno.h>
#include "RegionInstrumentation.h"

#undef LLVM_BINDIR
#include "config.h"
#if LLVM_VERSION_MINOR == 5
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/DebugInfo.h"
#else
#include "llvm/DebugInfo.h"
#include "llvm/Support/InstIterator.h"
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
struct OmpRegionInstrumentation : public FunctionPass {
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

  explicit OmpRegionInstrumentation(unsigned numLoops = ~0,
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

  bool insertOmpProbe(Module *mod, Function *F);
  bool choosePutMarkerOnFunction(std::string functionCall,
                                 std::string newFunctionName);
  virtual bool runOnFunction(Function &F);
  void insertMarker(Module *mod, Function *F, int place, Function *probe);
  LoadInst *insertMarkerInvocation(Module *mod, Function *F, int place,
                                   Function *probe, LoadInst *int32_1);

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

char OmpRegionInstrumentation::ID = 0;
static RegisterPass<OmpRegionInstrumentation>
X("omp-region-instrumentation", "Instrument parallel regions with probes",
  false, false);

/// Insert init probes
bool OmpRegionInstrumentation::runOnFunction(Function &F) {
  if (F.getName().find(".omp_microtask") != std::string::npos)
    return false;

  prepareInstrumentation(F, RegionFile, MeasureAppli, Mode, RequestedInvoc);
  Module *mod = F.getParent();

  insertOmpProbe(mod, &F);

  return true;
}

/// \brief Creates the CERE formated function name for the outlined region.
/// The syntax is __cere__filename__functionName__firstLine
std::string createFunctionName(Function *oldFunction, CallInst *callInst) {

  // Handle in vitro codelets
  std::size_t found = oldFunction->getName().find("__cere__");
  if (found != std::string::npos)
    return oldFunction->getName();

  std::string Separator = "_";

  // Get current module
  Module *mod = oldFunction->getParent();
  std::string newFunctionName;
  std::string File = mod->getModuleIdentifier();
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

/// Specify if the function call has to be wrapped
bool OmpRegionInstrumentation::choosePutMarkerOnFunction(
    std::string functionCall, std::string newFunctionName) {

  if (functionCall.find("__kmpc_fork_call") != std::string::npos) {
    if (RegionName == "all")
      return true;

    if (RegionName.compare(newFunctionName) == 0)
      return true;

    for (std::vector<std::string>::iterator it = LoopsToInstrument.begin(),
                                            et = LoopsToInstrument.end();
         it != et; it++) {
      std::size_t pos = it->find(newFunctionName);
      if (pos != std::string::npos) {
        return true;
      }
    }
  }
  return false;
}

/// Insert current marker to wrapper any call in the function F
void OmpRegionInstrumentation::insertMarker(Module *mod, Function *F, int place,
                                            Function *probe) {
  bool insertMarker;
  insertMarker = false;

  std::string functionCall;
  std::string probeName;

  std::vector<Value *> funcParameter;

  for (inst_iterator I = inst_begin(F), E = inst_end(F); I != E; ++I) {

    if (insertMarker) {
      Instruction *pinst = dyn_cast<Instruction>(&*I);
      CallInst::Create(probe, funcParameter, "", pinst);
      insertMarker = false;
    }

    if (CallInst *callInst = dyn_cast<CallInst>(&*I)) {
      if (callInst->getCalledFunction()) {
        functionCall = callInst->getCalledFunction()->getName();
        std::string newFunctionName = createFunctionName(F, callInst);

        if (choosePutMarkerOnFunction(functionCall, newFunctionName)) {
          funcParameter = createFunctionParameters(mod, newFunctionName, Mode,
                                                   RequestedInvoc);

          if (place == 0)
            CallInst::Create(probe, funcParameter, "", callInst);
          else
            insertMarker = true;
        }
      }
    }
  }
}

/// Insert current marker to wrapper any invocation call in the function F
LoadInst *OmpRegionInstrumentation::insertMarkerInvocation(
    Module *mod, Function *F, int place, Function *probe, LoadInst *int32_1) {
  bool insertMarker;
  insertMarker = false;

  std::string functionCall;
  std::string probeName;
  std::string newFunctionName;
  std::vector<Value *> funcParameter;

  for (inst_iterator I = inst_begin(F), E = inst_end(F); I != E; ++I) {

    if (insertMarker) {
      Instruction *pinst = dyn_cast<Instruction>(&*I);

      CallInst::Create(probe, funcParameter, "", pinst);
      insertMarker = false;
    }

    if (CallInst *callInst = dyn_cast<CallInst>(&*I)) {
      if (callInst->getCalledFunction()) {
        functionCall = callInst->getCalledFunction()->getName();
        newFunctionName = createFunctionName(F, callInst);

        if (choosePutMarkerOnFunction(functionCall, newFunctionName)) {

          if (place == 0) {
            // Create invocation counter
            GlobalVariable *gvar_int32_count = create_invocation_counter(mod);

            // Load the invocation counter
            LoadInst *int32_0 =
                new LoadInst(gvar_int32_count, "", false, (&*I));
            int32_0->setAlignment(4);
            // Increments it

            ConstantInt *const_int32_1 = ConstantInt::get(
                mod->getContext(), APInt(32, StringRef("1"), 10));
            BinaryOperator *int32_inc = BinaryOperator::Create(
                Instruction::Add, int32_0, const_int32_1, "inc", (&*I));
            // Save the value
            StoreInst *void_0 =
                new StoreInst(int32_inc, gvar_int32_count, false, (&*I));
            void_0->setAlignment(4);
            // Load the updated invocation counter
            int32_1 = new LoadInst(gvar_int32_count, "", false, (&*I));
            int32_1->setAlignment(4);
          }
          funcParameter = createFunctionParameters(mod, newFunctionName, Mode,
                                                   RequestedInvoc, int32_1);

          if (place == 0)
            CallInst::Create(probe, funcParameter, "", callInst);
          else
            insertMarker = true;
        }
      }
    }
  }
  return int32_1;
}

/// Insert start stop probes arround the parallel regions
bool OmpRegionInstrumentation::insertOmpProbe(Module *mod, Function *currFunc) {
  FunctionType *FuncTy = createFunctionType(mod);
  Function *func_start = createFunction(FuncTy, mod, "cere_markerStartRegion");
  Function *func_stop = createFunction(FuncTy, mod, "cere_markerStopRegion");

  // If requested invocation to measure is different than 0. It means we want
  // to record a particular invocation. We must then create a counter to count
  // the current invocation of the region
  if (RequestedInvoc != 0) {
    LoadInst *int32_1 =
        insertMarkerInvocation(mod, currFunc, 0, func_start, int32_1);
    insertMarkerInvocation(mod, currFunc, 1, func_stop, int32_1);
  } else {
    insertMarker(mod, currFunc, 0, func_start);
    insertMarker(mod, currFunc, 1, func_stop);
  }
  return true;
}
