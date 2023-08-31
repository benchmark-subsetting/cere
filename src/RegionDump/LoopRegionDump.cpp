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
// This file insert a dump function before a region to dump. This function takes
// as arguments every arguments of the top level function of the region. The
// dump function is defined in the memory_dump module.
//
//===----------------------------------------------------------------------===//

#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/LoopPass.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include <fstream>
#include <llvm/IR/Constants.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Type.h>
#include <set>
#include <sstream>
#include <string>

#include "RegionDump.h"

#define DEBUG_TYPE "region-dump"


using namespace llvm;

STATISTIC(RegionCounter, "Counts number of regions dumped");

extern cl::opt<std::string> RegionName;
extern cl::opt<std::string> RegionsFilename;
extern cl::opt<std::string> Invocations;


namespace {
struct LoopRegionDump : public FunctionPass {
  static char ID;
  unsigned NumLoops;

  std::string RegionToDump;
  std::string RegionString;
  std::string InvocationString;

  bool GlobalDump;
  bool ReadFromFile;

  std::vector<std::string> RegionsToDump;
  std::vector<std::string> InvocationsToDump;

  explicit LoopRegionDump(unsigned numLoops = ~0)
      : FunctionPass(ID), NumLoops(numLoops), RegionString(RegionName),
        InvocationString(Invocations), GlobalDump(false), ReadFromFile(false) {
    if (RegionToDump == "all")
      GlobalDump = true;

    // If regions are specified in CLI, take precedence over regions file
    if(!RegionName.empty()) {
      /* Parse regions list */
      RegionsToDump = split(RegionName, ';');

      /* Parse invocations list */
      // If not empty, read invocation string
      if(!InvocationString.empty()) {
        // Parse all region substrings
        std::vector<std::string> tmp_Strings = split(InvocationString, ';');
        for(std::string i : tmp_Strings) {
          InvocationsToDump.push_back(i);
        }
      }
      // Else, capture every first invocation by default
      else {
        std::vector<std::string> InvocationsToDump;
        for(int i=0; i<RegionsToDump.size(); i++) {
          InvocationsToDump.push_back("1");
        }
      }

    }
    // Else, fallback on the region file that has been specified
    else {
      if (!RegionsFilename.empty()) {
        std::string line;
        std::ifstream loopstream(RegionsFilename.c_str(), std::ios::in);
        if (loopstream.is_open()) {
          while (getline(loopstream, line))
            RegionsToDump.push_back(line);
          ReadFromFile = true;
        }
      }

    }
  }

  virtual bool runOnFunction(Function &F);
  bool visitLoop(Loop *L, Module *mod);

  virtual void getAnalysisUsage(AnalysisUsage &AU) const {
    AU.addRequired<DominatorTreeWrapperPass>();
    AU.addRequired<LoopInfoWrapperPass>();
  }
};
} // namespace

char LoopRegionDump::ID = 0;
static RegisterPass<LoopRegionDump> X("region-dump", "Dump regions");

/// \brief Called on each function int the current Module
bool LoopRegionDump::runOnFunction(Function &F) {
  Module *mod = F.getParent();

  // On Fortran code instrument on MAIN__ (after libgfortran init)
  // On other code instrument on main
  Function *Main = mod->getFunction("MAIN__");
  if (!Main) {
    Main = mod->getFunction("main");
  }
  if (Main) { // We are in the module with the main function
    IRBuilder<> builder(&(Main->front().front()));

    ConstantInt *const_int1_11;
    std::string funcName;
    if(split(Invocations, ',').size() > 1 || split(RegionName, ';').size() > 1) {
      funcName = "multi_dump_init";
    }
    else {
      funcName = "dump_init";
    }

    if (GlobalDump)
      const_int1_11 = ConstantInt::get(mod->getContext(),
                                       APInt(1, StringRef("-1"), 10)); // true
    else
      const_int1_11 = ConstantInt::get(mod->getContext(),
                                       APInt(1, StringRef("0"), 10)); // false

    Function *mainFunction = mod->getFunction(funcName);
    if (!mainFunction) {
      // Create signature: void func(int)
      std::vector<Type *> FuncTy_8_args;
      FuncTy_8_args.push_back(IntegerType::get(mod->getContext(), 1));
      FunctionType *FuncTy_8 = FunctionType::get(
          /*Result=*/Type::getVoidTy(mod->getContext()),
          /*Params=*/FuncTy_8_args,
          /*isVarArg=*/false);

      Function *mainFunction = Function::Create(
          FuncTy_8, GlobalValue::ExternalLinkage, funcName, mod);

      std::vector<Value *> void_16_params;
      void_16_params.push_back(const_int1_11);
      builder.CreateCall(mainFunction, void_16_params);
    }
  }

  LoopInfo &LI = getAnalysis<LoopInfoWrapperPass>().getLoopInfo();
  // Get all loops int the current function and visit them
  std::vector<Loop *> SubLoops(LI.begin(), LI.end());
  for (unsigned i = 0, e = SubLoops.size(); i != e; ++i)
    visitLoop(SubLoops[i], mod);
  return true;
}

bool LoopRegionDump::visitLoop(Loop *L, Module *mod) {
  // Ensure we are working only on outermost loops
  if (L->getLoopDepth() > 1)
    return false;

  // Get the function we are in
  Function *currFunc = L->getHeader()->getParent();

  // Are we in an isolated Region
  std::size_t found = currFunc->getName().find("__cere__");
  if (found == std::string::npos)
    return false;

  // If we want to dump a particular region, look if it's this one
  int regionIndex = dumpRequested(RegionsToDump, currFunc->getName());
  if (!GlobalDump && regionIndex == -1) {
    return false;
  } else if (ReadFromFile) {
    if (!(std::find(RegionsToDump.begin(), RegionsToDump.end(),
                    currFunc->getName()) != RegionsToDump.end()))
      return false;
  }

  /* Create dump function */
  Function *func_dump = mod->getFunction("dump");
  if (!func_dump) { // If function "dump" not found, creates it
    FunctionType *DumpFuncTy = createDumpFunctionType(mod);
    func_dump =
        Function::Create(DumpFuncTy, GlobalValue::ExternalLinkage, "dump", mod);
  }

  /* Create after_dump function */
  Function *func_after_dump = mod->getFunction("after_dump");
  if (!func_after_dump) { // If function "after_dump" not found, creates it
    // Create a void type
    std::vector<Type *> FuncTy_12_args;
    FunctionType *AfterDumpFuncTy = FunctionType::get(
        /*Result=*/Type::getVoidTy(mod->getContext()),
        /*Params=*/FuncTy_12_args,
        /*isVarArg=*/false);
    func_after_dump = Function::Create(
        AfterDumpFuncTy, GlobalValue::ExternalLinkage, "after_dump", mod);
  }
  // Get predecessor loop basic block
  BasicBlock *PredBB = L->getLoopPredecessor();
  // Get all exit blocks
  SmallVector<BasicBlock *, 8> exitblocks;
  L->getExitBlocks(exitblocks);

  if (PredBB == NULL || exitblocks.size() == 0)
    return false;

   ++RegionCounter;

  /* Insert dump calls (one for each invocation) */
  // Create arguments for the dump function
  std::vector<Value *> funcParameter =
  createDumpFunctionParameters(mod, currFunc, PredBB, InvocationsToDump[regionIndex]);

  // Insert dump call just before the region
  CallInst::Create(func_dump, funcParameter, "", &PredBB->back());

  /* Insert after_dump calls (once) in each exit blocks */
  for (SmallVectorImpl<BasicBlock *>::iterator I = exitblocks.begin(),
                                               E = exitblocks.end();
       I != E; ++I) {
    CallInst::Create(func_after_dump, "", &(*I)->front());
  }

  return true;
}
