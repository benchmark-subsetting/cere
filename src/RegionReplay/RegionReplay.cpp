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
 * Foobar is distributed in the hope that it will be useful,                 *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of            *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             *
 * GNU General Public License for more details.                              *
 *                                                                           *
 * You should have received a copy of the GNU General Public License         *
 * along with Foobar.  If not, see <http://www.gnu.org/licenses/>.           *
 *****************************************************************************/

//===----------------------------------------------------------------------===//
//
// This file insert functions needed to replay a region. It inserts a load
// function to load dumped memory. It also creates a wrapper to call the region
// to replay. These load functions are defined in the memory_dump module.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "region-replay"
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

STATISTIC(LoopCounter, "Counts number of replayed regions");

static cl::opt<std::string>
RegionName("region", cl::init("all"),
                  cl::value_desc("String"),
                  cl::desc("The region to replay"));
static cl::opt<int>
Invocation("invocation", cl::init(1),
                  cl::value_desc("Integer"),
                  cl::desc("Replay the specified invocation"));

namespace {
  struct RegionReplay : public FunctionPass {
    static char ID;
    unsigned NumLoops;
    int InvocationToReplay;
    std::string RegionToReplay;

    explicit RegionReplay(unsigned numLoops = ~0)
    : FunctionPass(ID), NumLoops(numLoops), RegionToReplay(RegionName), InvocationToReplay(Invocation) {}

    virtual bool runOnFunction(Function &F);
    bool visitLoop(Loop *L, Module *mod);

    virtual void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.addRequired<LoopInfo>();
      AU.addPreserved<LoopInfo>();
    }
  };
}

char RegionReplay::ID = 0;
static RegisterPass<RegionReplay> X("region-replay", "Replay regions");

/// Create signature for the load function:
/// void dump(char*, int, int, void* args[])
FunctionType* createLoadFunctionType(Module* mod)
{
  PointerType* PointerTy_4 = PointerType::get(IntegerType::get(mod->getContext(), 8), 0);
  PointerType* PointerTy_3 = PointerType::get(PointerTy_4, 0);

  std::vector<Type*>FuncTy_10_args;
  // Pointer for loop name
  FuncTy_10_args.push_back(PointerTy_4);
  // Int for the invocation to replay
  FuncTy_10_args.push_back(IntegerType::get(mod->getContext(), 32));
  // Int for the number of variable to load
  FuncTy_10_args.push_back(IntegerType::get(mod->getContext(), 32));
  // Pointer on the pointer array
  FuncTy_10_args.push_back(PointerTy_3);
  FunctionType* tmp = FunctionType::get(
    /*Result=*/Type::getVoidTy(mod->getContext()),
    /*Params=*/FuncTy_10_args,
    /*isVarArg=*/false);

  return tmp;
}

/// Creates arguments for the load function:
/// char* loopName
/// int InvocationToReplay
/// int number of variable arguments
/// void** array of adresses filled by the load lib
std::vector<Value*> createLoadFunctionParameters(Module* mod, Function* currFunc, BasicBlock* B, int N)
{
  PointerType* PointerTy_4 = PointerType::get(IntegerType::get(mod->getContext(), 8), 0);
  std::vector<Value*> params;

  //Get name of the loop to load
  Constant *param_name = ConstantDataArray::getString(mod->getContext(), currFunc->getName().str(), true);
  GlobalVariable* gvar_array__str = new GlobalVariable(/*Module=*/*mod,
    /*Type=*/param_name->getType(),
    /*isConstant=*/true,
    /*Linkage=*/GlobalValue::PrivateLinkage,
    /*Initializer=*/0, // has initializer, specified below
    /*Name=*/".str");
  gvar_array__str->setAlignment(1);
  ConstantInt* const_int32_10 = ConstantInt::get(mod->getContext(), APInt(32, StringRef("0"), 10));
  std::vector<Constant*> const_ptr_11_indices;
  const_ptr_11_indices.push_back(const_int32_10);
  const_ptr_11_indices.push_back(const_int32_10);
  Constant* const_ptr_11 = ConstantExpr::getGetElementPtr(gvar_array__str, const_ptr_11_indices);
  // Global Variable Definitions
  gvar_array__str->setInitializer(param_name);

  // The invocation to load
  ConstantInt* iterToLoad = ConstantInt::get(mod->getContext(), APInt(32, N, 10));

  // Get the number of argument we will have to load
  int nbArgs = 0;
  for (Function::arg_iterator args = currFunc->arg_begin(); args != currFunc->arg_end(); args++) nbArgs++;

  // Allocate a "void* args[nbArgs] array"
  ConstantInt* const_int_nbArgs = ConstantInt::get(mod->getContext(), APInt(32, nbArgs, 10));
  CastInst* int64_0 = new ZExtInst(const_int_nbArgs, IntegerType::get(mod->getContext(), 64), "", B);
  AllocaInst* ptr_vla = new AllocaInst(PointerTy_4, int64_0, "vla", B);

  // Push loop name + invocation to load + number of params + adresses array to load params
  params.push_back(const_ptr_11);
  params.push_back(iterToLoad);
  params.push_back(const_int_nbArgs);
  params.push_back(ptr_vla);

  return params;
}

/// \brief Creates the wrapper function signature: void wrapper(void)
Function* createWrapperFunction(Function* F, Module* M)
{
  //Create a void type
  std::vector<Type*> FuncTy_8_args;
  FunctionType* FuncTy_8 = FunctionType::get(
                      /*Result=*/Type::getVoidTy(M->getContext()),
                      /*Params=*/FuncTy_8_args,
                      /*isVarArg=*/false);
  //Create the wrapper function
  Function *tmp = Function::Create(FuncTy_8,
                      GlobalValue::ExternalLinkage,
                      "run" + F->getName().str(),
                      M);
  return tmp;
}

std::vector<Value*> createLoopParameters(Function* currFunc, Module* mod, AllocaInst* ptr_vla, BasicBlock* label_entry)
{
  std::vector<Value*> params;
  //get back adresses of loop parameters from the array filled by load
  int i=0;
  for (Function::arg_iterator args = currFunc->arg_begin(); args != currFunc->arg_end(); args++)
  {
    UnaryInstruction *ptr;
    ConstantInt* const_int64_35 = ConstantInt::get(mod->getContext(), APInt(32, i, 10));
    GetElementPtrInst* ptr_arrayidx = GetElementPtrInst::Create(ptr_vla, const_int64_35, "arrayidx", label_entry);
    LoadInst* ptr_69 = new LoadInst(ptr_arrayidx, "", false, label_entry); //get args[i]
    DEBUG(dbgs() << "Casting " << *ptr_69->getType() << " to " << *args->getType() << "\n");
    if(args->getType()->isPointerTy()) {
      ptr = new BitCastInst(ptr_69, args->getType(), "", label_entry); //cast
      //if args is a scalar, make a copy and give the copy adress to the func
      PointerType *pointerType = dyn_cast<PointerType>(args->getType());
      Type* elementType = pointerType->getElementType();
      if(!elementType->isArrayTy() && !elementType->isStructTy()) {
        AllocaInst* ptr_a_addr = new AllocaInst(args->getType(), "a", label_entry);
        AllocaInst* ptr_tmp = new AllocaInst(args->getType()->getPointerElementType(), "b", label_entry);
        AllocaInst* ptr_tmp2 = new AllocaInst(args->getType()->getPointerElementType(), "c", label_entry);
        //Store ptr adress
        new StoreInst(ptr, ptr_a_addr, false, label_entry);
        LoadInst* ptr_5 = new LoadInst(ptr_a_addr, "", false, label_entry);
        LoadInst* int32_6 = new LoadInst(ptr_5, "", false, label_entry);
        new StoreInst(int32_6, ptr_tmp, false, label_entry);
        LoadInst* int32_9 = new LoadInst(ptr_tmp, "", false, label_entry);
        new StoreInst(int32_9, ptr_tmp2, false, label_entry);
        ptr_tmp2->setName(args->getName());
        params.push_back(ptr_tmp2);
      }
      else {
        ptr->setName(args->getName());
        params.push_back(ptr);
      }
    }
    else {
      PointerType* PointerTy_3 = PointerType::get(args->getType(), 0);
      CastInst* ptr_70 = new BitCastInst(ptr_69, PointerTy_3, "", label_entry); //cast
      ptr = new LoadInst(ptr_70, "", false, label_entry);
      ptr->setName(args->getName());
      params.push_back(ptr);
    }
    i++;
  }
  return params;
}

/**Called on each function int the current Module**/
bool RegionReplay::runOnFunction(Function &F)
{
  Module* mod = F.getParent();

  // No main, no instrumentation!
  Function *Main = mod->getFunction("main");

  // Using fortran? ... this kind of works
  if (!Main)
    Main = mod->getFunction("MAIN__");

  if (Main) { //We are in the module with the main function
    std::string funcName="real_main";
    //~ConstantInt* const_int1_11 = ConstantInt::get(mod->getContext(), APInt(1, StringRef("0"), 10)); //false

    Function *mainFunction = mod->getFunction(funcName);
    if(!mainFunction) {
      std::vector<Type*> FuncTy_8_args;
      FunctionType* FuncTy_8 = FunctionType::get(
                              /*Result=*/Type::getVoidTy(mod->getContext()),
                              /*Params=*/FuncTy_8_args,
                              /*isVarArg=*/false);
      Function *mainFunction = Function::Create(FuncTy_8,
                      GlobalValue::ExternalLinkage,
                      funcName,
                      mod);
      std::vector<Value*> void_16_params;
      CallInst::Create(mainFunction, "", Main->begin()->begin());
    }
  }
  LoopInfo &LI = getAnalysis<LoopInfo>();
  //Get all loops int the current function and visit them
  std::vector<Loop*> SubLoops(LI.begin(), LI.end());
  for (unsigned i = 0, e = SubLoops.size(); i != e; ++i)
    visitLoop(SubLoops[i], mod);
  return true;
}

bool RegionReplay::visitLoop(Loop *L, Module *mod)
{
  // Ensure we are working only on outermost loops
  if(L->getLoopDepth() > 1) return false;

  // Get the function we are in
  Function* currFunc = L->getHeader()->getParent();

  // If we want to replay a particular region, look if it's this one
  if(RegionToReplay != currFunc->getName()) {

    return false;
  }

  // Create the wrapper function and add a basicBlock in it
  Function *wrapperFunction = createWrapperFunction(currFunc, mod);
  BasicBlock* label_entry = BasicBlock::Create(mod->getContext(), "entry", wrapperFunction, 0);

  // Now we create the load function and add a call in the wrapper function

  // Create the signature of the load function
  FunctionType* LoadFuncTy = createLoadFunctionType(mod);
  //Create the function load
  Function* func_load = mod->getFunction("load");
  if (!func_load) {
    func_load = Function::Create(
                /*Type=*/LoadFuncTy,
                /*Linkage=*/GlobalValue::ExternalLinkage,
                /*Name=*/"load", mod);
  }

  // Params for the load function
  std::vector<Value*> loadParams = createLoadFunctionParameters(mod, currFunc, label_entry, InvocationToReplay);

  //Add call to load
  CallInst::Create(func_load, loadParams, "", label_entry);

  // Ok now we have adresses of variables used by the loop in ptr_vla array,
  // so we have to read it, and create parameters for the loop
  std::vector<Value*> loopParams = createLoopParameters(currFunc, mod, cast<AllocaInst>(loadParams.back()), label_entry);

  // Call the function with the extracted loop
  CallInst::Create(currFunc, loopParams, "", label_entry);

  // We must add an anti dead code function
  std::vector<Type*> FuncADCE_args;
  // Signature: void anti_dead_code_elimination(int, ...);
  FuncADCE_args.push_back(IntegerType::get(mod->getContext(), 32));
  FunctionType* FuncADCE_type = FunctionType::get(
    /*Result=*/Type::getVoidTy(mod->getContext()),
    /*Params=*/FuncADCE_args,
    /*isVarArg=*/true);
  // create antiDCE function
  Function *antiDeadCodeFunction = Function::Create(FuncADCE_type,
                      GlobalValue::ExternalLinkage,
                      "anti_dead_code_elimination",
                      mod);
  antiDeadCodeFunction->addFnAttr(Attribute::NoInline);
  // AntiDCE function takes exactly the same parameters then the loop
  // + the number of parameters as we pass them as variable argument
  ConstantInt* nbParam = ConstantInt::get(mod->getContext(), APInt(32, loopParams.size(), 10));
  loopParams.insert(loopParams.begin(), nbParam);
  //Add the call
  CallInst::Create(antiDeadCodeFunction, loopParams, "", label_entry);
  //Add a return instruction for wrapper function
  ReturnInst::Create(mod->getContext(), label_entry);

  return true;
}
