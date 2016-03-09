/*****************************************************************************
 * This file is part of CERE.                                                *
 *                                                                           *
 * Copyright (c) 2013-2016, Universite de Versailles St-Quentin-en-Yvelines  *
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
// This file renames internal global variable with a unique name to avoid
// conflicts when setting the visibility to public.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "global-rename"
#include "llvm/IR/Module.h"
#include "llvm/Support/Debug.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/Pass.h"

using namespace llvm;

namespace {
  struct GlobalRename : public ModulePass {
    bool changed;
    static char ID; // Pass identification, replacement for typeid
    GlobalRename() : ModulePass(ID), changed(false) {}
    virtual bool runOnModule(Module &M);
  };
}

char GlobalRename::ID = 0;
static RegisterPass<GlobalRename> X("global-rename", "Rename global variables", false, false);

bool GlobalRename::runOnModule(Module &M) {
  //Iterate over global variables
  for (Module::global_iterator I = M.global_begin(), E = M.global_end();
                                                      I != E; ++I) {
    //If this variable is private or internal rename it
    if ( (I->getLinkage() ==  GlobalValue::PrivateLinkage) || (I->getLinkage() == GlobalValue::InternalLinkage) ) {
      //Give the module name as variable name, setName handle collisions
      I->setName(M.getModuleIdentifier());
      changed = true;
    }
  }
  return changed;
}
