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
// A utility to support extracting code from one function into its own
// stand-alone function.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_TRANSFORMS_UTILS_REGION_EXTRACTOR_H
#define LLVM_TRANSFORMS_UTILS_REGION_EXTRACTOR_H

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/Twine.h"

namespace llvm {
class BasicBlock;
class DominatorTree;
class Function;
class Loop;
class Module;
class RegionNode;
class Type;
class Value;

/// \brief Utility class for extracting code into a new function.
///
/// This utility provides a simple interface for extracting some sequence of
/// code into its own function, replacing it with a call to that function. It
/// also provides various methods to query about the nature and result of
/// such a transformation.
///
/// The rough algorithm used is:
/// 1) Find both the inputs and outputs for the extracted region.
/// 2) Pass the inputs as arguments, remapping them within the extracted
///    function to arguments.
/// 3) Add allocas for any scalar outputs, adding all of the outputs' allocas
///    as arguments, and inserting stores to the arguments for any scalars.
class RegionExtractor {
  typedef SetVector<Value *> ValueSet;

  // Various bits of state computed on construction.
  DominatorTree *const DT;
  const bool AggregateArgs;
  std::string Separator;
  std::string LoopFileInfos;

  // Bits of intermediate state computed at various phases of extraction.
  SetVector<BasicBlock *> Blocks;
  unsigned NumExitBlocks;
  std::string RegionName;
  bool Pcere;
  bool ProfileApp;
  Type *RetTy;

public:
  /// \brief Create a code extractor for a single basic block.
  ///
  /// In this formation, we don't require a dominator tree. The given basic
  /// block is set up for extraction.
  RegionExtractor(BasicBlock *BB, std::string regionName = "",
                  bool profileApp = false, bool pcere = false,
                  bool AggregateArgs = false);

  /// \brief Create a code extractor for a sequence of blocks.
  ///
  /// Given a sequence of basic blocks where the first block in the sequence
  /// dominates the rest, prepare a code extractor object for pulling this
  /// sequence out into its new function. When a DominatorTree is also given,
  /// extra checking and transformations are enabled.
  RegionExtractor(ArrayRef<BasicBlock *> BBs, DominatorTree *DT = 0,
                  bool AggregateArgs = false);

  /// \brief Create a code extractor for a loop body.
  ///
  /// Behaves just like the generic code sequence constructor, but uses the
  /// block sequence of the loop.
  RegionExtractor(DominatorTree &DT, Loop &L, std::string regionName = "",
                  bool profileApp = false, bool pcere = false,
                  bool AggregateArgs = false);

  /// \brief Create a code extractor for a region node.
  ///
  /// Behaves just like the generic code sequence constructor, but uses the
  /// block sequence of the region node passed in.
  RegionExtractor(DominatorTree &DT, const RegionNode &RN,
                  bool AggregateArgs = false);

  /// \brief Perform the extraction, returning the new function.
  ///
  /// Returns zero when called on a CodeExtractorAll instance where isEligible
  /// returns false.
  Function *extractCodeRegion();

  /// \brief Test whether this code extractor is eligible.
  ///
  /// Based on the blocks used when constructing the code extractor,
  /// determine whether it is eligible for extraction.
  bool isEligible() const { return !Blocks.empty(); }

  /// \brief Compute the set of input values and output values for the code.
  ///
  /// These can be used either when performing the extraction or to evaluate
  /// the expected size of a call to the extracted function. Note that this
  /// work cannot be cached between the two as once we decide to extract
  /// a code sequence, that sequence is modified, including changing these
  /// sets, before extraction occurs. These modifications won't have any
  /// significant impact on the cost however.
  void findInputsOutputs(ValueSet &Inputs, ValueSet &Outputs) const;

  void add_region_to_file(std::string, std::string, std::string, std::string,
                          std::string, std::string = "");
  bool is_region_in_file(std::string, std::fstream &);

private:
  void severSplitPHINodes(BasicBlock *&Header);
  void splitReturnBlocks();

  std::string createFunctionName(Function *oldFunction, BasicBlock *header);
  std::string removeExtension(const std::string &);
  std::string removeChar(std::string str, const char toReplace,
                         const char replacer);
  std::string updateFileFormat(std::string str);

  Function *constructFunction(const ValueSet &inputs, const ValueSet &outputs,
                              BasicBlock *header, BasicBlock *newRootNode,
                              BasicBlock *newHeader, Twine newFunctionName,
                              Function *oldFunction, Module *M);

  void moveCodeToFunction(Function *newFunction);
  void removeWrongMetadata(Function *newFunction);
  void emitCallAndSwitchStatement(Function *newFunction, BasicBlock *newHeader,
                                  ValueSet &inputs, ValueSet &outputs);
};
}

#endif
