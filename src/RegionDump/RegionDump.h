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
// This file contains the useful prototypes for the dump.
//
//===----------------------------------------------------------------------===//

namespace llvm {

/// Creates parameters for the dump function
/// char* RegionName
/// int InvocationToDump
/// int number of variable arguments
/// ... variables used by the original region
std::vector<Value *> createDumpFunctionParameters(Module *mod,
                                                  Function *currFunc,
                                                  BasicBlock *PredBB, int N);

/// \brief Create a pointer to string \p s
Constant *createStringValue(Module *mod, std::string &s);

/// Create signature for the dump function
/// void dump(char*, int, int, ...)
FunctionType *createDumpFunctionType(Module *mod);
}
