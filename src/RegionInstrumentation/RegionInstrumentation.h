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
// This file contains the useful prototypes for the instrumentation.
//
//===----------------------------------------------------------------------===//

namespace llvm {

/// \brief Removes extension from \p filename.
std::string removeExtension(const std::string &filename);

/// \brief Removes character \p toReplace by \p replacer in \p str.
std::string removeChar(std::string str, const char toReplace,
                       const char replacer);

/// Create The function signature for cere_marker Start and Stop. This function
/// takes a char*, a boolean and two integers as arguments and returns nothing.
FunctionType *createFunctionType(Module *mod);

/// Create a function "name" in the
/// current module. This function
/// takes \p FuncTy as signature
Function *createFunction(FunctionType *FuncTy, Module *mod, std::string name);

/// \brief Definition of atexit function
Function *createAtexit(Module *mod);

/// \brief Update file format.
std::string updateFileFormat(std::string str);

/// \brief Creates a global integer variable
GlobalVariable *create_invocation_counter(Module *mod);

/// Create a vector of parameter to fit the signature of cere init and close
/// probes.
std::vector<Value *> createInitParameters(Module *mod, std::string regionN);

void prepareInstrumentation(Function &F, std::string fileN, bool MeasureA,
                            bool mode, int RequestedI);

/// Create a vector of parameter to fit the signature of cere start and stop
/// probes.
std::vector<Value *> createFunctionParameters(Module *mod,
                                              std::string newFunctionName,
                                              int mode, int RequestedI,
                                              LoadInst *int32 = NULL);
}
