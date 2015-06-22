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
// This file contains the useful prototypes for the replay.
//
//===----------------------------------------------------------------------===//

namespace llvm {

/// Create signature for the load function:
/// void dump(char*, int, int, void* args[])
FunctionType *createLoadFunctionType(Module *mod);

/// Creates arguments for the load function:
/// char* loopName
/// int InvocationToReplay
/// int number of variable arguments
/// void** array of adresses filled by the load lib
std::vector<Value *> createLoadFunctionParameters(Module *mod,
                                                  Function *currFunc,
                                                  BasicBlock *B, int N);

/// \brief Creates the wrapper function signature: void wrapper(void)
Function *createWrapperFunction(Function *F, Module *M);

std::vector<Value *> createLoopParameters(Function *currFunc, Module *mod,
                                          AllocaInst *ptr_vla,
                                          BasicBlock *label_entry);
}
