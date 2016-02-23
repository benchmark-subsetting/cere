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
// This file contains the useful functions for the replay.
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
#include <string>
#include <sstream>
#include <fstream>
#include <set>
#include "RegionReplay.h"

#if LLVM_VERSION_MINOR == 5
#include "llvm/IR/InstIterator.h"
#else
#include "llvm/Support/InstIterator.h"
#endif

using namespace llvm;

cl::opt<std::string> RegionName("region", cl::init("all"),
                                cl::value_desc("String"),
                                cl::desc("The region to replay"));
cl::opt<int> Invocation("invocation", cl::init(1), cl::value_desc("Integer"),
                        cl::desc("Replay the specified invocation"));

namespace llvm {

/// Create signature for the load function:
/// void dump(char*, int, int, void* args[])
FunctionType *createLoadFunctionType(Module *mod) {
  PointerType *PointerTy_4 =
      PointerType::get(IntegerType::get(mod->getContext(), 8), 0);
  PointerType *PointerTy_3 = PointerType::get(PointerTy_4, 0);

  std::vector<Type *> FuncTy_10_args;
  // Pointer for loop name
  FuncTy_10_args.push_back(PointerTy_4);
  // Int for the invocation to replay
  FuncTy_10_args.push_back(IntegerType::get(mod->getContext(), 32));
  // Int for the number of variable to load
  FuncTy_10_args.push_back(IntegerType::get(mod->getContext(), 32));
  // Pointer on the pointer array
  FuncTy_10_args.push_back(PointerTy_3);
  FunctionType *tmp = FunctionType::get(
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
std::vector<Value *> createLoadFunctionParameters(Module *mod,
                                                  Function *currFunc,
                                                  BasicBlock *B, int N) {
  PointerType *PointerTy_4 =
      PointerType::get(IntegerType::get(mod->getContext(), 8), 0);
  std::vector<Value *> params;

  // Get name of the loop to load
  Constant *param_name = ConstantDataArray::getString(
      mod->getContext(), currFunc->getName().str(), true);
  GlobalVariable *gvar_array__str =
      new GlobalVariable(/*Module=*/*mod,
                         /*Type=*/param_name->getType(),
                         /*isConstant=*/true,
                         /*Linkage=*/GlobalValue::PrivateLinkage,
                         /*Initializer=*/0, // has initializer, specified below
                         /*Name=*/".str");
  gvar_array__str->setAlignment(1);
  ConstantInt *const_int32_10 =
      ConstantInt::get(mod->getContext(), APInt(32, StringRef("0"), 10));
  std::vector<Constant *> const_ptr_11_indices;
  const_ptr_11_indices.push_back(const_int32_10);
  const_ptr_11_indices.push_back(const_int32_10);
  Constant *const_ptr_11 =
      ConstantExpr::getGetElementPtr(gvar_array__str, const_ptr_11_indices);
  // Global Variable Definitions
  gvar_array__str->setInitializer(param_name);

  // The invocation to load
  ConstantInt *iterToLoad =
      ConstantInt::get(mod->getContext(), APInt(32, N, 10));

  // Get the number of argument we will have to load
  int nbArgs = 0;
  for (Function::arg_iterator args = currFunc->arg_begin();
       args != currFunc->arg_end(); args++)
    nbArgs++;

  // Allocate a "void* args[nbArgs] array"
  ConstantInt *const_int_nbArgs =
      ConstantInt::get(mod->getContext(), APInt(32, nbArgs, 10));
  CastInst *int64_0 = new ZExtInst(
      const_int_nbArgs, IntegerType::get(mod->getContext(), 64), "", B);
  AllocaInst *ptr_vla = new AllocaInst(PointerTy_4, int64_0, "vla", B);

  // Push loop name + invocation to load + number of params + adresses array to
  // load params
  params.push_back(const_ptr_11);
  params.push_back(iterToLoad);
  params.push_back(const_int_nbArgs);
  params.push_back(ptr_vla);

  return params;
}

/// \brief Creates the wrapper function signature: void wrapper(void)
Function *createWrapperFunction(Function *F, Module *M) {
  // Create a void type
  std::vector<Type *> FuncTy_8_args;
  FunctionType *FuncTy_8 = FunctionType::get(
      /*Result=*/Type::getVoidTy(M->getContext()),
      /*Params=*/FuncTy_8_args,
      /*isVarArg=*/false);
  // Create the wrapper function
  Function *tmp = Function::Create(FuncTy_8, GlobalValue::ExternalLinkage,
                                   "run" + F->getName().str(), M);
  return tmp;
}

std::vector<Value *> createLoopParameters(Function *currFunc, Module *mod,
                                          AllocaInst *ptr_vla,
                                          BasicBlock *label_entry) {
  std::vector<Value *> params;
  // get back adresses of loop parameters from the array filled by load
  int i = 0;
  for (Function::arg_iterator args = currFunc->arg_begin();
       args != currFunc->arg_end(); args++) {
    UnaryInstruction *ptr;
    ConstantInt *const_int64_35 =
        ConstantInt::get(mod->getContext(), APInt(32, i, 10));
    GetElementPtrInst *ptr_arrayidx = GetElementPtrInst::Create(
        ptr_vla, const_int64_35, "arrayidx", label_entry);
    LoadInst *ptr_69 =
        new LoadInst(ptr_arrayidx, "", false, label_entry); // get args[i]
    DEBUG(dbgs() << "Casting " << *ptr_69->getType() << " to "
                 << *args->getType() << "\n");
    if (args->getType()->isPointerTy()) {
      ptr = new BitCastInst(ptr_69, args->getType(), "", label_entry); // cast
      // if args is a scalar, make a copy and give the copy adress to the func
      PointerType *pointerType = dyn_cast<PointerType>(args->getType());
      Type *elementType = pointerType->getElementType();
      if (!elementType->isArrayTy() && !elementType->isStructTy()) {
        AllocaInst *ptr_a_addr =
            new AllocaInst(args->getType(), "a", label_entry);
        AllocaInst *ptr_tmp = new AllocaInst(
            args->getType()->getPointerElementType(), "b", label_entry);
        AllocaInst *ptr_tmp2 = new AllocaInst(
            args->getType()->getPointerElementType(), "c", label_entry);
        // Store ptr adress
        new StoreInst(ptr, ptr_a_addr, false, label_entry);
        LoadInst *ptr_5 = new LoadInst(ptr_a_addr, "", false, label_entry);
        LoadInst *int32_6 = new LoadInst(ptr_5, "", false, label_entry);
        new StoreInst(int32_6, ptr_tmp, false, label_entry);
        LoadInst *int32_9 = new LoadInst(ptr_tmp, "", false, label_entry);
        new StoreInst(int32_9, ptr_tmp2, false, label_entry);
        ptr_tmp2->setName(args->getName());
        params.push_back(ptr_tmp2);
      } else {
        ptr->setName(args->getName());
        params.push_back(ptr);
      }
    } else {
      PointerType *PointerTy_3 = PointerType::get(args->getType(), 0);
      CastInst *ptr_70 =
          new BitCastInst(ptr_69, PointerTy_3, "", label_entry); // cast
      ptr = new LoadInst(ptr_70, "", false, label_entry);
      ptr->setName(args->getName());
      params.push_back(ptr);
    }
    i++;
  }
  return params;
}
}
