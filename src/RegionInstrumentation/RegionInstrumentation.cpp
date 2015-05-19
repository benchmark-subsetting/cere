//===- RegionInstrumentation.cpp - Instrument a region with probes --------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the ... License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file insert an init function at the beginning of programm. The close
// function is registered through atexit. It also inserts start and stop probes
// around a requested region. These functions can be linked with any user
// library.
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

#undef LLVM_BINDIR 
#include "config.h"
#if LLVM_VERSION_MINOR == 4
#include "llvm/IR/Constants.h"
#endif

using namespace llvm;
enum{VIVO, VITRO};

STATISTIC(LoopCounter, "Counts number of regions instrumented");

static cl::opt<std::string>
IsolateRegion("instrument-region", cl::init("all"),
    cl::value_desc("string"),
    cl::desc("instrument-region instruments the region with probes"));
static cl::opt<std::string>
LoopsFilename("regions-file", cl::init(""),
    cl::value_desc("filename"),
    cl::desc("File with regions to instrument"));
static cl::opt<bool>
AppMeasure("instrument-app", cl::init(false),
    cl::value_desc("Boolean"),
    cl::desc("Put instrumentation at the beginning and the end of the application"));
static cl::opt<bool>
Replay("replay", cl::init(false),
    cl::value_desc("Boolean"),
    cl::desc("Specify if we are in replay or not"));
static cl::opt<int>
Invocation("invocation", cl::init(0),
    cl::value_desc("Integer"),
    cl::desc("Measure only the requested region invocation"));

namespace {
struct RegionInstrumentation : public FunctionPass {
  static char ID;
  unsigned NumLoops;
  std::string RegionName;
  std::string Separator;
  std::string RegionFile;
  int Mode;
  int RequestedInvoc;
  bool ReadFromFile;
  bool MeasureAppli;
  std::vector<std::string> LoopsToInstrument;
  LoopInfo *LI;  // The current loop information

  explicit RegionInstrumentation(unsigned numLoops = ~0,
                                 const std::string &regionName = "")
  : FunctionPass(ID), NumLoops(numLoops), RegionName(regionName), Mode(VIVO),
  Separator("_"), RegionFile(LoopsFilename), RequestedInvoc(Invocation),
  MeasureAppli(AppMeasure) {
    if (Replay) Mode = VITRO;
    if (regionName.empty()) RegionName = IsolateRegion;
      if (RegionFile.empty()) ReadFromFile = false;
      else {
        // We read a file, we need to empty RegionName
        RegionName = "";
        // Open the file and add regions in LoopsToInstrument
        std::string line;
        std::ifstream loopstream;
        loopstream.open(RegionFile.c_str(), std::ios::in);
        if(loopstream.is_open()) {
          while(getline(loopstream, line)) {
            LoopsToInstrument.push_back(line);
          }
          loopstream.close();
          ReadFromFile = true;
        }
        else {
          errs() << "Can't open file > " << RegionFile.c_str() << " : " <<
          strerror(errno) << "\n";
          ReadFromFile = false;
        }
      }
    }

  virtual bool runOnFunction(Function &F);
  std::string createFunctionName(Loop *L, Function *oldFunction);
  bool visitLoop(Loop *L, Module *mod);
  std::vector<Value*> createFunctionParameters(Module* mod,
                                               std::string newFunctionName,
                                               LoadInst* int32 = NULL);
  std::vector<Value*> createInitParameters(Module* mod);

  virtual void getAnalysisUsage(AnalysisUsage &AU) const {
    AU.addRequiredID(BreakCriticalEdgesID);
    AU.addRequired<LoopInfo>();
    AU.addPreserved<LoopInfo>();
    AU.addRequired<DominatorTree>();
  }
};
}

char RegionInstrumentation::ID = 0;
static RegisterPass<RegionInstrumentation> X("region-instrumentation",
                                             "Instrument regions with probes",
                                             false,
                                             false);

/// \brief Removes extension from \p filename.
std::string removeExtension( const std::string &filename) {
  size_t lastdot = filename.find_last_of(".");
  if (lastdot == std::string::npos) return filename;
  return filename.substr(0, lastdot);
}

/// \brief Removes character \p toReplace by \p replacer in \p str.
std::string removeChar(std::string str,
                       const char toReplace,
                       const char replacer) {
  replace(str.begin(), str.end(), toReplace, replacer);
  return str;
}

/// Create The function signature for cere_marker Start and Stop. This function
/// takes a char*, a boolean and two integers as arguments and returns nothing.
FunctionType* createFunctionType(Module* mod) {
  // Arguments vector
  std::vector<Type*> FuncTy_args;

  // Char* for the regionName
  FuncTy_args.push_back(PointerType::get(IntegerType::get(mod->getContext(), 8),
                                         0));
  // Boolean for invivo or invitro mod
  FuncTy_args.push_back(IntegerType::get(mod->getContext(), 1));
  // Integer for the requested invocation to measure
  FuncTy_args.push_back(IntegerType::get(mod->getContext(), 32));
  // Integer for the current invocation
  FuncTy_args.push_back(IntegerType::get(mod->getContext(), 32));

  FunctionType* tmp = FunctionType::get(
  /*Result=*/Type::getVoidTy(mod->getContext()),
  /*Params=*/FuncTy_args,
  /*isVarArg=*/false);

  return tmp;
}

/// Create a function "name" in the
/// current module. This function
/// takes \p FuncTy as signature
Function* createFunction(FunctionType* FuncTy, Module* mod, std::string name) {
  Function* tmp = mod->getFunction(name);
  // If the function is not yet defined, creates it.
  if (!tmp)
  {
    tmp = Function::Create(
    /*Type=*/FuncTy,
    /*Linkage=*/GlobalValue::ExternalLinkage,
    /*Name=*/name, mod);
    tmp->setCallingConv(CallingConv::C);
  }
  return tmp;
}

/// \brief Definition of atexit function
Function* createAtexit(Module* mod) {

  std::vector<Type*>FuncTy_01_args;
  FunctionType* FuncTy_01 = FunctionType::get(
    /*Result=*/Type::getVoidTy(mod->getContext()),
    /*Params=*/FuncTy_01_args,
    /*isVarArg=*/false);

  PointerType* PointerTy_01 = PointerType::get(FuncTy_01, 0);

  std::vector<Type*>FuncTy_02_args;
  FuncTy_02_args.push_back(PointerTy_01);
  FunctionType* FuncTy_02 = FunctionType::get(
    /*Result=*/IntegerType::get(mod->getContext(), 32),
    /*Params=*/FuncTy_02_args,
    /*isVarArg=*/false);

  //Create atexit definition
  Function* func_atexit = mod->getFunction("atexit");
  if (!func_atexit) {
    func_atexit = Function::Create(
    /*Type=*/FuncTy_02,
    /*Linkage=*/GlobalValue::ExternalLinkage,
    /*Name=*/"atexit", mod); // (external, no body)
    func_atexit->setCallingConv(CallingConv::C);
  }
  return func_atexit;
}

/// Create a vector of parameter to fit the signature of cere start and stop
/// probes. (char*, bool, int, int)
std::vector<Value*> RegionInstrumentation::createFunctionParameters(Module* mod,
                                           std::string newFunctionName,
                                           LoadInst* int32) {
  // LoopName
  // Get current function name
  Constant *param_name = ConstantDataArray::getString(mod->getContext(),
                                                      newFunctionName,
                                                      true);
  GlobalVariable* gvar_array__str = new GlobalVariable(/*Module=*/*mod,
  /*Type=*/param_name->getType(),
  /*isConstant=*/true,
  /*Linkage=*/GlobalValue::PrivateLinkage,
  /*Initializer=*/0,
  /*Name=*/".str");
  gvar_array__str->setAlignment(1);

  ConstantInt* const_int32_0 = ConstantInt::get(mod->getContext(),
                                                APInt(32, StringRef("0"), 10));
  std::vector<Constant*> const_ptr_indices;
  const_ptr_indices.push_back(const_int32_0);
  const_ptr_indices.push_back(const_int32_0);
  Constant* const_ptr_0 = ConstantExpr::getGetElementPtr(gvar_array__str,
                                                         const_ptr_indices);

  // Set vivo/vitro boolean
  ConstantInt* const_int1;
  if(Mode == VIVO)
    const_int1 = ConstantInt::get(mod->getContext(),
                                  APInt(1, StringRef("-1"), 10)); // true
  else
    const_int1 = ConstantInt::get(mod->getContext(),
                                  APInt(1, StringRef("0"), 10)); // false

  // Store requested invocation
  ConstantInt* const_int32_1 = ConstantInt::get(mod->getContext(),
                                                APInt(32, RequestedInvoc, 10));

  // Global Variable Definitions
  gvar_array__str->setInitializer(param_name);

  std::vector<Value*> void_params;
  void_params.push_back(const_ptr_0); //LoopName
  void_params.push_back(const_int1); //Vivo/Vitro boolean
  void_params.push_back(const_int32_1); //requested
  if(RequestedInvoc == 0)//We want to measure all invocations
    void_params.push_back(const_int32_1);
  else
    void_params.push_back(int32);
  return void_params;
}

/// Create a vector of parameter to fit the signature of cere init and close
/// probes. (char*)
std::vector<Value*> RegionInstrumentation::createInitParameters(Module* mod) {

  // Get region filename
  Constant *param_name = ConstantDataArray::getString(mod->getContext(),
                                                      RegionFile,
                                                      true);
  GlobalVariable* gvar_array__str = new GlobalVariable(/*Module=*/*mod,
  /*Type=*/param_name->getType(),
  /*isConstant=*/true,
  /*Linkage=*/GlobalValue::PrivateLinkage,
  /*Initializer=*/0,
  /*Name=*/".str");
  gvar_array__str->setAlignment(1);

  ConstantInt* const_int32_0 = ConstantInt::get(mod->getContext(),
                                                APInt(32, StringRef("0"), 10));
  std::vector<Constant*> const_ptr_indices;
  const_ptr_indices.push_back(const_int32_0);
  const_ptr_indices.push_back(const_int32_0);
  Constant* const_ptr_0 = ConstantExpr::getGetElementPtr(gvar_array__str,
                                                         const_ptr_indices);

  // Global Variable Definitions
  gvar_array__str->setInitializer(param_name);

  std::vector<Value*> void_params;
  void_params.push_back(const_ptr_0); //Region filename

  return void_params;
}

/// \brief Creates the CERE formated function name for the outlined region.
/// The syntax is __cere__filename__functionName__firstLine
std::string RegionInstrumentation::createFunctionName(Loop *L,
                                                      Function *oldFunction) {
  //Get current module
  Module *mod = oldFunction->getParent();
  std::string module_name = mod->getModuleIdentifier();

  std::string newFunctionName;
  std::ostringstream oss;
  BasicBlock *firstBB = L->getBlocks()[0];

  if (MDNode *firstN = firstBB->front().getMetadata("dbg")) {
    DILocation firstLoc(firstN);
    oss << firstLoc.getLineNumber();
    std::string firstLine = oss.str();
    std::string Original_location = firstLoc.getFilename().str();
    std::string File = module_name;
    std::string path = firstLoc.getDirectory();

    newFunctionName = "__cere__" + removeExtension(File) + Separator +
                      oldFunction->getName().str() + Separator + firstLine;
    newFunctionName = removeChar(newFunctionName, '-', '_');
    newFunctionName = removeChar(newFunctionName, '/', '_');
    newFunctionName = removeChar(newFunctionName, '+', '_');
    newFunctionName = removeChar(newFunctionName, '.', '_');
  }

  return newFunctionName;
}

/// \brief Creates a global integer variable
GlobalVariable* create_invocation_counter(Module *mod)
{
  GlobalVariable* gvar_int32_count = new GlobalVariable(/*Module=*/*mod, 
  /*Type=*/IntegerType::get(mod->getContext(), 32),
  /*isConstant=*/false,
  /*Linkage=*/GlobalValue::InternalLinkage,
  /*Initializer=*/0,
  /*Name=*/"cere_invocation_counter");
  gvar_int32_count->setAlignment(4);
  ConstantInt* const_int32 = ConstantInt::get(mod->getContext(),
                                              APInt(32, StringRef("0"), 10));
  gvar_int32_count->setInitializer(const_int32);
  return gvar_int32_count;
}

/// \brief This function find if the string \p newFunctionName is present in
/// the regions file
bool RegionInstrumentation::runOnFunction(Function &F) {
  Module* mod = F.getParent();

  std::vector<Type*>FuncTy_args;
  FunctionType* FuncTy_Close = FunctionType::get(
                    /*Result=*/Type::getVoidTy(mod->getContext()),
                    /*Params=*/FuncTy_args,
                    /*isVarArg=*/false);

  // Char* for the regionfile
  FuncTy_args.push_back(PointerType::get(IntegerType::get(mod->getContext(), 8),
                                         0));
  FunctionType* FuncTy_Init = FunctionType::get(
                    /*Result=*/Type::getVoidTy(mod->getContext()),
                    /*Params=*/FuncTy_args,
                    /*isVarArg=*/false);

  // No main, no instrumentation!
  Function *Main = mod->getFunction("main");

  // Using fortran? ... this kind of works
  if (!Main)
    Main = mod->getFunction("MAIN__");

  if (Main) {
    //Get atexit function
    Function* func_atexit = createAtexit(mod);

    // Get entry basic block of the main function
    BasicBlock *firstBB = dyn_cast<BasicBlock>(Main->begin());
    std::vector<BasicBlock*> ReturningBlocks;
    for(Function::iterator I = Main->begin(), E = Main->end(); I != E; ++I) {
      if (isa<ReturnInst>(I->getTerminator())) ReturningBlocks.push_back(I);
    }

    // If we want to measure the whole application cycle, insert start
    // in the entry basic block and stop in each exit basic blocks.
    if(MeasureAppli) {
      FunctionType* FuncTy_1 = createFunctionType(mod);
      std::vector<Value*> funcParameter = createFunctionParameters(mod,
                                                                   "main");
      Function *startFunction = mod->getFunction("cere_markerStartRegion");
      Function *stopFunction = mod->getFunction("cere_markerStopRegion");
      // Create start function if it does not exists yet
      if(!startFunction) {
        Function* func_start = createFunction(FuncTy_1, mod,
                                              "cere_markerStartRegion");
        CallInst *start = CallInst::Create(func_start, funcParameter, "",
                                             &firstBB->front());
      }
      // Create stop function if it does not exists yet
      if(!stopFunction) {
        Function* func_stop = createFunction(FuncTy_1, mod,
                                             "cere_markerStopRegion");
        //We must insert stop probe in all exits blocks
        for (std::vector<BasicBlock*>::iterator I = ReturningBlocks.begin(),
                                                E = ReturningBlocks.end();
                                                I != E; ++I) {
          CallInst::Create(func_stop, funcParameter, "", &(*I)->back());
        }
      }
    }
    // Add cere init as the first instruction of the entry basic block.
    // Cere close must be added via atexit.
    Function *initFunction = mod->getFunction("cere_markerInit");
    if(!initFunction) {
      Function *initFunction = Function::Create(FuncTy_Init,
                                                GlobalValue::ExternalLinkage,
                                                "cere_markerInit",
                                                mod);
      CallInst::Create(initFunction, createInitParameters(mod), "", &firstBB->front());
      DEBUG(dbgs() << "Init successfuly inserted in main function\n");
    }
    Function *closeFunction = mod->getFunction("cere_markerClose");
    if(!closeFunction) {
      Function *closeFunction = Function::Create(FuncTy_Close,
                                                 GlobalValue::ExternalLinkage,
                                                 "cere_markerClose",
                                                 mod);
      // Register close marker with atexit
      CallInst::Create(func_atexit, closeFunction, "", &firstBB->front());
      DEBUG(dbgs() << "Close successfuly inserted in main function\n");
    }
  }
  // If we want to instrument a region, visit every loops.
  if(!MeasureAppli) {
    LoopInfo &LI = getAnalysis<LoopInfo>();
    std::vector<Loop*> SubLoops(LI.begin(), LI.end());
    for (unsigned i = 0, e = SubLoops.size(); i != e; ++i)
      visitLoop(SubLoops[i], mod);
  }
  return true;
}

/// \brief Runs on each loop and inserts probes around the requested ones
bool RegionInstrumentation::visitLoop(Loop *L, Module *mod) {
  // Get current function
  Function* currFunc = L->getHeader()->getParent();

  // Ensure that we are working on outermost loops
  if(L->getLoopDepth() > 1) return false; 
  if(!L->isLoopSimplifyForm()) {
    DEBUG(dbgs() << "Loop found, but not in simple form...\n");
    return false;
  }

  // If the loop is inside a CERE function it means it has already been
  // extracted. Otherwise just generate the name of the corresponding region
  std::string newFunctionName;
  std::size_t found = currFunc->getName().find("__cere__");
  if (found == std::string::npos) newFunctionName = createFunctionName(L,
                                                                    currFunc);
  else newFunctionName = currFunc->getName();
  if(newFunctionName.empty()) return false;

  // If it's not the requested region, and we don't read regions from a file
  if (RegionName != "all" && RegionName != newFunctionName && !ReadFromFile) {
    return false;
  }
  // If we read regions to instrument from a file, check if the current region
  // is in that file
  else if(ReadFromFile) {
    bool found=false;
    for(std::vector<std::string>::iterator it = LoopsToInstrument.begin(),
                                           et = LoopsToInstrument.end();
                                           it != et; it++) {
      std::size_t pos = it->find(newFunctionName);
      if (pos != std::string::npos) {
        found=true;
        break;
      }
    }
    if(!found) return false;
  }

  ++LoopCounter;
  // Create start and stop functions signatures
  FunctionType* FuncTy = createFunctionType(mod);
  Function* func_start = createFunction(FuncTy, mod, "cere_markerStartRegion");
  Function* func_stop = createFunction(FuncTy, mod, "cere_markerStopRegion");

  // Get predecessor and exit basic blocks of the current loop
  BasicBlock *PredBB = L->getLoopPredecessor();
  SmallVector<BasicBlock*,8> exitblocks;
  L->getExitBlocks(exitblocks);
  if(PredBB == NULL || exitblocks.size() == 0)
  {
    errs() << "Can't find Prdecessor or successor basick block for region "<<
               RegionName << "\n";
    DEBUG(dbgs() << "Can't find predecessor or successor basick block for \
                    region "<< RegionName << "\n");
    return false;
  }
  std::vector<Value*> funcParameter;

  // If requested invocation to measure is different than 0. It means we want
  // to record a particular invocation. We must then create a counter to count
  // the current invocation of the region
  if(RequestedInvoc != 0)
  {
    // Create invocation counter
    GlobalVariable* gvar_int32_count = create_invocation_counter(mod);

    // Load the invocation counter
    LoadInst* int32_0 = new LoadInst(gvar_int32_count, "", false,
                                     &PredBB->back());
    int32_0->setAlignment(4);
    // Increments it
    ConstantInt* const_int32_1 = ConstantInt::get(mod->getContext(),
                                                  APInt(32, StringRef("1"), 10));
    BinaryOperator* int32_inc = BinaryOperator::Create(Instruction::Add,
                                                       int32_0,
                                                       const_int32_1,
                                                       "inc",
                                                       &PredBB->back());
    // Save the value
    StoreInst* void_0 = new StoreInst(int32_inc, gvar_int32_count, false,
                                      &PredBB->back());
    void_0->setAlignment(4);
    // Load the updated invocation counter
    LoadInst* int32_1 = new LoadInst(gvar_int32_count, "", false,
                                     &PredBB->back());
    int32_1->setAlignment(4);

    // Create function parameters
    funcParameter = createFunctionParameters(mod, newFunctionName, int32_1);
  }
  else {
    // Create function parameters
    funcParameter = createFunctionParameters(mod, newFunctionName);
  }

  // Add the call of cere start probe as the last instruction of the
  // predecessor basic block
  CallInst::Create(func_start, funcParameter, "", &PredBB->back());
  // Add the call of cere stop probe as the last instruction of all exit
  // basic block
  for (SmallVectorImpl<BasicBlock *>::iterator I = exitblocks.begin(),
                                               E = exitblocks.end();
                                               I != E; ++I) {
    CallInst::Create(func_stop, funcParameter, "", &((*I)->back()));
  }
    return true;
}
