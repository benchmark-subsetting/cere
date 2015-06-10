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

#define DEBUG_TYPE "region-dump"
#include "llvm/Support/Debug.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/LoopPass.h"
#include <llvm/IR/Constants.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/DataLayout.h>
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/InstIterator.h"
#include <string>
#include <sstream>
#include <fstream>
#include <set>

using namespace llvm;

STATISTIC(RegionCounter, "Counts number of regions dumped");

static cl::opt<std::string>
RegionName("region", cl::init("all"),
                  cl::value_desc("String"),
                  cl::desc("The region to dump"));
static cl::opt<std::string>
RegionsFilename("regions-file", cl::init(""),
                  cl::value_desc("File"),
                  cl::desc("File with regions to dump"));
static cl::opt<int>
Invocation("invocation", cl::init(1),
                  cl::value_desc("Integer"),
                  cl::desc("Dump the specified invocation"));

namespace {
  struct RegionDump : public FunctionPass {
    static char ID;
    std::string RegionToDump;
    unsigned NumLoops;
    int InvocationToDump;
    bool ReadFromFile;
    bool GlobalDump;
    std::vector<std::string> RegionsToDump;

    explicit RegionDump(unsigned numLoops = ~0)
    : FunctionPass(ID), NumLoops(numLoops), RegionToDump(RegionName),
      InvocationToDump(Invocation),
    GlobalDump(false), ReadFromFile(false) {
      if (RegionToDump == "all") GlobalDump = true;
      if (!RegionsFilename.empty()) {
        std::string line;
        std::ifstream loopstream(RegionsFilename.c_str(), std::ios::in);
        if(loopstream.is_open()) {
          while(getline(loopstream,line))
            RegionsToDump.push_back(line);
          ReadFromFile = true;
        }
      }
    }

    virtual bool runOnFunction(Function &F);
    bool visitLoop(Loop *L, Module *mod);

    virtual void getAnalysisUsage(AnalysisUsage &AU) const {
        AU.addRequired<LoopInfo>();
        AU.addPreserved<LoopInfo>();
    }
  };
}

char RegionDump::ID = 0;
static RegisterPass<RegionDump> X("region-dump", "Dump regions");

/// Create signature for the dump function
/// void dump(char*, int, int, ...)
FunctionType* createDumpFunctionType(Module* mod) {
  PointerType* PointerTy_0 = PointerType::get(IntegerType::get(mod->getContext(),
                                                               8), 0);
  std::vector<Type*> FuncTy_args;
  // Char* for the region name
  FuncTy_args.push_back(PointerTy_0);
  // Integer for the invocation to dump
  FuncTy_args.push_back(IntegerType::get(mod->getContext(), 32));
  // Integer for the number of va args
  FuncTy_args.push_back(IntegerType::get(mod->getContext(), 32));
  FunctionType* tmp = FunctionType::get(
    /*Result=*/Type::getVoidTy(mod->getContext()),
    /*Params=*/FuncTy_args,
    /*isVarArg=*/true);
  return tmp;
}

/// \brief Create a pointer to string \p s
Constant* createStringValue(Module* mod, std::string &s) {

  Constant *const_array_0 = ConstantDataArray::getString(mod->getContext(),
                                                         s,
                                                         true);
  GlobalVariable* gvar_array__str = new GlobalVariable(/*Module=*/*mod,
    /*Type=*/const_array_0->getType(),
    /*isConstant=*/true,
    /*Linkage=*/GlobalValue::PrivateLinkage,
    /*Initializer=*/0, // has initializer, specified below
    /*Name=*/".str");

  gvar_array__str->setAlignment(1);
  ConstantInt* const_int32_0 = ConstantInt::get(mod->getContext(),
                                                APInt(32, StringRef("0"), 10));
  std::vector<Constant*> const_ptr_0_indices;
  const_ptr_0_indices.push_back(const_int32_0);
  const_ptr_0_indices.push_back(const_int32_0);
  gvar_array__str->setInitializer(const_array_0);

  return ConstantExpr::getGetElementPtr(gvar_array__str, const_ptr_0_indices);
}

/// Creates parameters for the dump function
/// char* RegionName
/// int InvocationToDump
/// int number of variable arguments
/// ... variables used by the original region
std::vector<Value*> createDumpFunctionParameters(Module* mod,
                                                 Function* currFunc,
                                                 BasicBlock* PredBB,
                                                 int N) {
  // The requested region has been outlined into a function.
  // Give adresses of its arguments to the dump function.
  std::vector<Value*> params;
  for (Function::arg_iterator args = currFunc->arg_begin();
                              args != currFunc->arg_end(); args++)
  {
    // If argument is a value get its address
    if(args->getType()->isPointerTy() == false) {
      AllocaInst* ptr_args = new AllocaInst(args->getType(), args->getName(),
                                            &PredBB->back());
      new StoreInst(args, ptr_args, false, &PredBB->back());
      // If this argument does not have a name, create one
      if((ptr_args->getName().str()).empty()) {
        std::ostringstream ss;
        ss << params.size();
        ptr_args->setName(ss.str()+".addr");
      }
      params.push_back(ptr_args);
    }
    // If argument is already a pointer, just give the adress
    else {
      Argument* ptr_args = args;
      if((ptr_args->getName().str()).empty()) {
        std::ostringstream ss;
        ss << params.size();
        ptr_args->setName(ss.str()+".addr");
      }
      params.push_back(ptr_args);
    }
  }
  ConstantInt* nbParam = ConstantInt::get(mod->getContext(),
                                          APInt(32, params.size(), 10));
  ConstantInt* iterToDump = ConstantInt::get(mod->getContext(),
                                             APInt(32, N, 10));

  params.insert(params.begin(), nbParam);
  params.insert(params.begin(), iterToDump);
  /*Convert the regionName as a C type string*/
  std::string tmp = currFunc->getName().str();
  params.insert(params.begin(), createStringValue(mod, tmp));

  return params;
}

/// \brief Called on each function int the current Module
bool RegionDump::runOnFunction(Function &F) {
  Module* mod = F.getParent();

  // On Fortran code instrument on MAIN__ (after libgfortran init)
  // On other code instrument on main
  Function *Main = mod->getFunction("MAIN__");
  if (!Main) {
    Main = mod->getFunction("main");
  }
  if (Main) { //We are in the module with the main function
    ConstantInt* const_int1_11;
    std::string funcName="dump_init";
    if(GlobalDump)
      const_int1_11 = ConstantInt::get(mod->getContext(),
                                       APInt(1, StringRef("-1"), 10)); //true
    else
      const_int1_11 = ConstantInt::get(mod->getContext(),
                                       APInt(1, StringRef("0"), 10)); //false

    Function *mainFunction = mod->getFunction(funcName);
    if(!mainFunction) {
      //Create signature: void func(int)
      std::vector<Type*> FuncTy_8_args;
      FuncTy_8_args.push_back(IntegerType::get(mod->getContext(), 1));
      FunctionType* FuncTy_8 = FunctionType::get(
                              /*Result=*/Type::getVoidTy(mod->getContext()),
                              /*Params=*/FuncTy_8_args,
                              /*isVarArg=*/false);
      Function *mainFunction = Function::Create(FuncTy_8,
                        GlobalValue::ExternalLinkage,
                        funcName,
                        mod);
      std::vector<Value*> void_16_params;
      void_16_params.push_back(const_int1_11);
      CallInst::Create(mainFunction, void_16_params, "", Main->begin()->begin());
    }
  }
  LoopInfo &LI = getAnalysis<LoopInfo>();
  //Get all loops int the current function and visit them
  std::vector<Loop*> SubLoops(LI.begin(), LI.end());
  for (unsigned i = 0, e = SubLoops.size(); i != e; ++i)
    visitLoop(SubLoops[i], mod);
  return true;
}

bool RegionDump::visitLoop(Loop *L, Module *mod)
{
  // Ensure we are working only on outermost loops
  if(L->getLoopDepth() > 1) return false;

  // Get the function we are in
  Function* currFunc = L->getHeader()->getParent();

  // Are we in an isolated Region
  std::size_t found = currFunc->getName().find("__cere__");
  if (found == std::string::npos) return false;

  // If we want to dump a particular region, look if it's this one
  if(!GlobalDump && RegionToDump != currFunc->getName()) {
    return false;
  }
  else if(ReadFromFile) {
    if(!(std::find(RegionsToDump.begin(), RegionsToDump.end(),
         currFunc->getName()) != RegionsToDump.end()))
    return false;
  }
  // Create dump function
  Function* func_dump = mod->getFunction("dump");
  if (!func_dump) { // If function "dump" not found, creates it
    FunctionType* DumpFuncTy = createDumpFunctionType(mod);
    func_dump = Function::Create(DumpFuncTy,
                                GlobalValue::ExternalLinkage,
                                "dump", mod);
  }

  // Create after_dump function
  Function* func_after_dump = mod->getFunction("after_dump");
  if (!func_after_dump) { // If function "after_dump" not found, creates it
    //Create a void type
    std::vector<Type*> FuncTy_12_args;
    FunctionType* AfterDumpFuncTy = FunctionType::get(
                              /*Result=*/Type::getVoidTy(mod->getContext()),
                              /*Params=*/FuncTy_12_args,
                              /*isVarArg=*/false);
    func_after_dump = Function::Create(AfterDumpFuncTy,
                                      GlobalValue::ExternalLinkage,
                                      "after_dump", mod);
  }
  // Get predecessor loop basic block
  BasicBlock *PredBB = L->getLoopPredecessor();
  // Get all exit blocks
  SmallVector<BasicBlock*,8> exitblocks;
  L->getExitBlocks(exitblocks);

  if(PredBB == NULL || exitblocks.size() == 0) return false;

  ++RegionCounter;
  // Create arguments for the dump function
  std::vector<Value*> funcParameter = createDumpFunctionParameters(mod,
                                                              currFunc,
                                                              PredBB,
                                                              InvocationToDump);

  // Call dup function just before the region
  CallInst::Create(func_dump, funcParameter, "", &PredBB->back());
  // Call after dump in each exit blocks
  for (SmallVectorImpl<BasicBlock *>::iterator I = exitblocks.begin(),
                                               E = exitblocks.end();
                                               I != E; ++I) {
    CallInst::Create(func_after_dump, "", &((*I)->front()));
  }

  return true;
}
