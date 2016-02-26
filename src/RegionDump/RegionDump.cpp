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
// This file contains the useful functions for the dump.
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
#include <string>
#include <sstream>
#include <fstream>
#include <set>
#include "RegionDump.h"

#if LLVM_VERSION_MINOR == 5
#include "llvm/IR/InstIterator.h"
#else
#include "llvm/Support/InstIterator.h"
#endif

using namespace llvm;

cl::opt<std::string> RegionName("region", cl::init("all"),
                                cl::value_desc("String"),
                                cl::desc("The region to dump"));
cl::opt<std::string> RegionsFilename("regions-file", cl::init(""),
                                     cl::value_desc("File"),
                                     cl::desc("File with regions to dump"));
cl::opt<int> Invocation("invocation", cl::init(1), cl::value_desc("Integer"),
                        cl::desc("Dump the specified invocation"));

namespace llvm {

/// Create signature for the dump function
/// void dump(char*, int, int, ...)
FunctionType *createDumpFunctionType(Module *mod) {
  PointerType *PointerTy_0 =
      PointerType::get(IntegerType::get(mod->getContext(), 8), 0);
  std::vector<Type *> FuncTy_args;
  // Char* for the region name
  FuncTy_args.push_back(PointerTy_0);
  // Integer for the invocation to dump
  FuncTy_args.push_back(IntegerType::get(mod->getContext(), 32));
  // Integer for the number of va args
  FuncTy_args.push_back(IntegerType::get(mod->getContext(), 32));
  FunctionType *tmp = FunctionType::get(
      /*Result=*/Type::getVoidTy(mod->getContext()),
      /*Params=*/FuncTy_args,
      /*isVarArg=*/true);
  return tmp;
}

/// \brief Create a pointer to string \p s
Constant *createStringValue(Module *mod, std::string &s) {

  Constant *const_array_0 =
      ConstantDataArray::getString(mod->getContext(), s, true);
  GlobalVariable *gvar_array__str =
      new GlobalVariable(/*Module=*/*mod,
                         /*Type=*/const_array_0->getType(),
                         /*isConstant=*/true,
                         /*Linkage=*/GlobalValue::PrivateLinkage,
                         /*Initializer=*/0, // has initializer, specified below
                         /*Name=*/".str");

  gvar_array__str->setAlignment(1);
  ConstantInt *const_int32_0 =
      ConstantInt::get(mod->getContext(), APInt(32, StringRef("0"), 10));
  std::vector<Constant *> const_ptr_0_indices;
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
std::vector<Value *> createDumpFunctionParameters(Module *mod,
                                                  Function *currFunc,
                                                  BasicBlock *PredBB, int N) {
  // The requested region has been outlined into a function.
  // Give adresses of its arguments to the dump function.
  std::vector<Value *> params;
  for (Function::arg_iterator args = currFunc->arg_begin();
       args != currFunc->arg_end(); args++) {
    // If argument is a value get its address
    if (args->getType()->isPointerTy() == false) {
      AllocaInst *ptr_args =
          new AllocaInst(args->getType(), args->getName(), &PredBB->back());
      new StoreInst(args, ptr_args, false, &PredBB->back());
      // If this argument does not have a name, create one
      if ((ptr_args->getName().str()).empty()) {
        std::ostringstream ss;
        ss << params.size();
        ptr_args->setName(ss.str() + ".addr");
      }
      params.push_back(ptr_args);
    }
    // If argument is already a pointer, just give the adress
    else {
      Argument *ptr_args = args;
      if ((ptr_args->getName().str()).empty()) {
        std::ostringstream ss;
        ss << params.size();
        ptr_args->setName(ss.str() + ".addr");
      }
      params.push_back(ptr_args);
    }
  }
  ConstantInt *nbParam =
      ConstantInt::get(mod->getContext(), APInt(32, params.size(), 10));
  ConstantInt *iterToDump =
      ConstantInt::get(mod->getContext(), APInt(32, N, 10));

  params.insert(params.begin(), nbParam);
  params.insert(params.begin(), iterToDump);
  /*Convert the regionName as a C type string*/
  std::string tmp = currFunc->getName().str();
  params.insert(params.begin(), createStringValue(mod, tmp));

  return params;
}
}
