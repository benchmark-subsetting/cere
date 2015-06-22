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
// This file contains the useful functions for the instrumentation.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "region-instrumentation"
#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/LoopPass.h"
#include <llvm/IR/Instructions.h>
#include "llvm/Transforms/Scalar.h"
#include "llvm/IR/Module.h"
#include "llvm/DebugInfo.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/CommandLine.h"
#include <fstream>
#include <sstream>
#include <errno.h>

#include "RegionInstrumentation.h"
#undef LLVM_BINDIR
#include "config.h"
#if LLVM_VERSION_MINOR == 4
#include "llvm/IR/Constants.h"
#endif

using namespace llvm;

cl::opt<std::string> IsolateRegion(
    "instrument-region", cl::init("all"), cl::value_desc("string"),
    cl::desc("instrument-region instruments the region with probes"));
cl::opt<std::string> LoopsFilename("regions-file", cl::init(""),
                                   cl::value_desc("filename"),
                                   cl::desc("File with regions to instrument"));
cl::opt<bool> AppMeasure(
    "instrument-app", cl::init(false), cl::value_desc("Boolean"),
    cl::desc(
        "Put instrumentation at the beginning and the end of the application"));
cl::opt<bool> Replay("replay", cl::init(false), cl::value_desc("Boolean"),
                     cl::desc("Specify if we are in replay or not"));
cl::opt<int>
    Invocation("invocation", cl::init(0), cl::value_desc("Integer"),
               cl::desc("Measure only the requested region invocation"));

namespace llvm {
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

/// Create The function signature for cere_marker Start and Stop. This function
/// takes a char*, a boolean and two integers as arguments and returns nothing.
FunctionType *createFunctionType(Module *mod) {
  // Arguments vector
  std::vector<Type *> FuncTy_args;

  // Char* for the regionName
  FuncTy_args.push_back(
      PointerType::get(IntegerType::get(mod->getContext(), 8), 0));
  // Boolean for invivo or invitro mod
  FuncTy_args.push_back(IntegerType::get(mod->getContext(), 1));
  // Integer for the requested invocation to measure
  FuncTy_args.push_back(IntegerType::get(mod->getContext(), 32));
  // Integer for the current invocation
  FuncTy_args.push_back(IntegerType::get(mod->getContext(), 32));

  FunctionType *tmp = FunctionType::get(
      /*Result=*/Type::getVoidTy(mod->getContext()),
      /*Params=*/FuncTy_args,
      /*isVarArg=*/false);

  return tmp;
}

/// Create a function "name" in the
/// current module. This function
/// takes \p FuncTy as signature
Function *createFunction(FunctionType *FuncTy, Module *mod, std::string name) {
  Function *tmp = mod->getFunction(name);
  // If the function is not yet defined, creates it.
  if (!tmp) {
    tmp = Function::Create(
        /*Type=*/FuncTy,
        /*Linkage=*/GlobalValue::ExternalLinkage,
        /*Name=*/name, mod);
    tmp->setCallingConv(CallingConv::C);
  }
  return tmp;
}

/// \brief Definition of atexit function
Function *createAtexit(Module *mod) {

  std::vector<Type *> FuncTy_01_args;
  FunctionType *FuncTy_01 = FunctionType::get(
      /*Result=*/Type::getVoidTy(mod->getContext()),
      /*Params=*/FuncTy_01_args,
      /*isVarArg=*/false);

  PointerType *PointerTy_01 = PointerType::get(FuncTy_01, 0);

  std::vector<Type *> FuncTy_02_args;
  FuncTy_02_args.push_back(PointerTy_01);
  FunctionType *FuncTy_02 = FunctionType::get(
      /*Result=*/IntegerType::get(mod->getContext(), 32),
      /*Params=*/FuncTy_02_args,
      /*isVarArg=*/false);

  // Create atexit definition
  Function *func_atexit = mod->getFunction("atexit");
  if (!func_atexit) {
    func_atexit = Function::Create(
        /*Type=*/FuncTy_02,
        /*Linkage=*/GlobalValue::ExternalLinkage,
        /*Name=*/"atexit", mod); // (external, no body)
    func_atexit->setCallingConv(CallingConv::C);
  }
  return func_atexit;
}

/// \brief Update file format.
std::string updateFileFormat(std::string str) {
  str = removeChar(str, '-', '_');
  str = removeChar(str, '/', '_');
  str = removeChar(str, '+', '_');
  str = removeChar(str, '.', '_');
  return str;
}

/// \brief Creates a global integer variable
GlobalVariable *create_invocation_counter(Module *mod) {
  GlobalVariable *gvar_int32_count =
      new GlobalVariable(/*Module=*/*mod,
                         /*Type=*/IntegerType::get(mod->getContext(), 32),
                         /*isConstant=*/false,
                         /*Linkage=*/GlobalValue::InternalLinkage,
                         /*Initializer=*/0,
                         /*Name=*/"cere_invocation_counter");
  gvar_int32_count->setAlignment(4);
  ConstantInt *const_int32 =
      ConstantInt::get(mod->getContext(), APInt(32, StringRef("0"), 10));
  gvar_int32_count->setInitializer(const_int32);
  return gvar_int32_count;
}
}
