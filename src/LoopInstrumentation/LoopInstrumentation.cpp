#define DEBUG_TYPE "loop-instrumentation"
#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/LoopPass.h"
#include "llvm/Analysis/LoopInfo.h"
#include <llvm/IR/Constants.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Type.h>
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/IR/Module.h"
#include "llvm/DebugInfo.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/InstIterator.h"
#include "llvm/Support/CommandLine.h"
#include <fstream>
#include <sstream>
#include <iostream>

using namespace llvm;
enum{VIVO, VITRO};

STATISTIC(LoopCounter, "Counts number of loops instrumented");

static cl::opt<std::string>
Tool("tool", cl::init("rdtsc"),
                  cl::value_desc("String"),
                  cl::desc("Tool name to instrument the region(rdtsc, likwid...)"));
static cl::opt<std::string>
IsolateLoop("instrument-loop", cl::init("all"),
                  cl::value_desc("loopname"),
                  cl::desc("loop-instrument will only instrument this region"));
static cl::opt<std::string>
LoopsFilename("loops-file", cl::init(""),
                  cl::value_desc("filename"),
                  cl::desc("File with regions to instrument"));
static cl::opt<std::string>
TraceLoop("loop-to-trace", cl::init(""),
                  cl::value_desc("String"),
                  cl::desc("instrumentation-mode allow to trace and record all invocations measures for specified region(s)"));
static cl::opt<bool>
AppMeasure("instrument-app", cl::init(false),
                  cl::value_desc("Boolean"),
                  cl::desc("if True, application time will be measured"));
static cl::opt<int>
Invocation("invocation", cl::init(0),
                  cl::value_desc("Integer"),
                  cl::desc("Measure only the Nth region invocation"));

std::string removeExtension( const std::string &filename) {
    size_t lastdot = filename.find_last_of(".");
    if (lastdot == std::string::npos) return filename;
    return filename.substr(0, lastdot);
}

std::string removeChar(std::string str, const char toReplace, const char replacer)
{
    replace(str.begin(), str.end(), toReplace, replacer);
    return str;
}

namespace {
  struct LoopRDTSCInstrumentation : public FunctionPass {
    static char ID;
    unsigned NumLoops;
    std::string Loopname;
    std::string separator;
    std::string Loopfile;
    std::string InstruTool;
    std::string LoopToTrace;
    int Mode;
    int invoc;
    bool readFromFile;
    bool measureAppli;
    std::vector<std::string> loopsToInstrument;
    LoopInfo *LI;  // The current loop information

    explicit LoopRDTSCInstrumentation(int mode = VITRO, unsigned numLoops = ~0, const std::string &loopname = "")
    : FunctionPass(ID), NumLoops(numLoops), Loopname(loopname), separator("_"), Loopfile(LoopsFilename), InstruTool(Tool), LoopToTrace(TraceLoop), Mode(mode), invoc(Invocation), measureAppli(AppMeasure) {
        if (loopname.empty()) Loopname = IsolateLoop;
        if (Loopfile.empty()) readFromFile = false;
        else {
            std::string line;
            std::ifstream loopstream(Loopfile.c_str(), std::ios::in);
            if(loopstream.is_open()) {
                while(getline(loopstream, line)) {
                    line = removeChar(line, '#', ' ');
                    loopsToInstrument.push_back(line);
                }
                loopstream.close();
                readFromFile = true;
            }
            else {
                errs() << "Cannot open file >" << Loopfile << "<\n";
                readFromFile = false;
            }
        }
    }

    virtual bool runOnFunction(Function &F);
    std::string createFunctionName(Loop *L, Function *oldFunction);
    bool visitLoop(Loop *L, Module *mod);
    std::vector<Value*> createFunctionParameters(Module* mod, std::string newFunctionName, LoadInst* int32_39 = NULL);
    std::vector<Value*> createInitParameters(Module* mod);

    virtual void getAnalysisUsage(AnalysisUsage &AU) const {
      AU.addRequiredID(BreakCriticalEdgesID);
      AU.addRequired<LoopInfo>();
      AU.addPreserved<LoopInfo>();
      AU.addRequired<DominatorTree>();
    }
  };
}

char LoopRDTSCInstrumentation::ID = 0;
static RegisterPass<LoopRDTSCInstrumentation> X("vitro-loop-instrumentation", "Instrument in-vitro loops", false, false);

namespace {
    struct VivoLoopRDTSCInstrumentation: public LoopRDTSCInstrumentation {
        static char ID;
        VivoLoopRDTSCInstrumentation() : LoopRDTSCInstrumentation(VIVO) {}
    };
}

char VivoLoopRDTSCInstrumentation::ID = 0;
static RegisterPass<VivoLoopRDTSCInstrumentation> Y("vivo-loop-instrumentation", "Instrument in-vivo loops", false, false);

/**Create char* Type**/
FunctionType* createFunctionType(Module* mod)
{
    PointerType* PointerTy_4 = PointerType::get(IntegerType::get(mod->getContext(), 8), 0);
    PointerType* PointerTy_5 = PointerType::get(IntegerType::get(mod->getContext(), 8), 0);
 
    std::vector<Type*>FuncTy_6_args;
    FuncTy_6_args.push_back(PointerTy_4); //char* for the instrumentation tool
    FuncTy_6_args.push_back(PointerTy_5); //char* for the regionName
    FuncTy_6_args.push_back(IntegerType::get(mod->getContext(), 1)); //bool
    FuncTy_6_args.push_back(IntegerType::get(mod->getContext(), 1)); //bool
    FuncTy_6_args.push_back(IntegerType::get(mod->getContext(), 32)); //int
    FuncTy_6_args.push_back(IntegerType::get(mod->getContext(), 32)); //global variable
    FunctionType* tmp = FunctionType::get(
    /*Result=*/Type::getVoidTy(mod->getContext()),
    /*Params=*/FuncTy_6_args,
    /*isVarArg=*/false);
    return tmp;
}

/**Create a function "name" in the
 * current module. This function
 * takes no argument and return nothing
**/
Function* createFunction(FunctionType* FuncTy_0, Module* mod, std::string name)
{
    Function* tmp = mod->getFunction(name);
    if (!tmp)
    {
        tmp = Function::Create(
        /*Type=*/FuncTy_0,
        /*Linkage=*/GlobalValue::ExternalLinkage,
        /*Name=*/name, mod); 
        tmp->setCallingConv(CallingConv::C);
    }
    return tmp;
}

std::vector<Value*> LoopRDTSCInstrumentation::createFunctionParameters(Module* mod, std::string newFunctionName, LoadInst* int32_39)
{
    //Tool
    Constant *param_name_tool = ConstantDataArray::getString(mod->getContext(), InstruTool, true); //get Tool
    GlobalVariable* gvar_array__str_tool = new GlobalVariable(/*Module=*/*mod,
    /*Type=*/param_name_tool->getType(),
    /*isConstant=*/true,
    /*Linkage=*/GlobalValue::PrivateLinkage,
    /*Initializer=*/0, // has initializer, specified below
    /*Name=*/".str");
    gvar_array__str_tool->setAlignment(1);

    ConstantInt* const_int32_9 = ConstantInt::get(mod->getContext(), APInt(32, StringRef("0"), 10));
    std::vector<Constant*> const_ptr_10_indices;
    const_ptr_10_indices.push_back(const_int32_9);
    const_ptr_10_indices.push_back(const_int32_9);
    Constant* const_ptr_10 = ConstantExpr::getGetElementPtr(gvar_array__str_tool, const_ptr_10_indices);
    
    //LoopName
    Constant *param_name = ConstantDataArray::getString(mod->getContext(), newFunctionName, true); //get current function name
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

    //Set Trace boolean
    ConstantInt* const_int1_11;
    if(newFunctionName == LoopToTrace) const_int1_11 = ConstantInt::get(mod->getContext(), APInt(1, StringRef("-1"), 10));
    else const_int1_11 = ConstantInt::get(mod->getContext(), APInt(1, StringRef("0"), 10));

    //Set global boolean
    ConstantInt* const_int;
    const_int = ConstantInt::get(mod->getContext(), APInt(1, StringRef("0"), 10)); //always false

    //Store requested invocation
    ConstantInt* const_int32_21 = ConstantInt::get(mod->getContext(), APInt(32, invoc, 10));

    // Global Variable Definitions
    gvar_array__str->setInitializer(param_name);
    gvar_array__str_tool->setInitializer(param_name_tool);
    std::vector<Value*> void_16_params;
    void_16_params.push_back(const_ptr_10); //Tool
    void_16_params.push_back(const_ptr_11); //LoopName
    void_16_params.push_back(const_int1_11); //Trace boolean
    void_16_params.push_back(const_int); //Global measure boolean
    void_16_params.push_back(const_int32_21); //requested
    if(invoc == 0)//We want to measure all invocations
        void_16_params.push_back(const_int32_21);
    else
        void_16_params.push_back(int32_39);
    return void_16_params;
}

std::vector<Value*> LoopRDTSCInstrumentation::createInitParameters(Module* mod)
{
    //Tool
    Constant *param_name_tool = ConstantDataArray::getString(mod->getContext(), InstruTool, true); //get Tool
    GlobalVariable* gvar_array__str_tool = new GlobalVariable(/*Module=*/*mod,
    /*Type=*/param_name_tool->getType(),
    /*isConstant=*/true,
    /*Linkage=*/GlobalValue::PrivateLinkage,
    /*Initializer=*/0, // has initializer, specified below
    /*Name=*/".str");
    gvar_array__str_tool->setAlignment(1);

    ConstantInt* const_int32_9 = ConstantInt::get(mod->getContext(), APInt(32, StringRef("0"), 10));
    std::vector<Constant*> const_ptr_10_indices;
    const_ptr_10_indices.push_back(const_int32_9);
    const_ptr_10_indices.push_back(const_int32_9);
    Constant* const_ptr_10 = ConstantExpr::getGetElementPtr(gvar_array__str_tool, const_ptr_10_indices);

    gvar_array__str_tool->setInitializer(param_name_tool);
    std::vector<Value*> void_16_params;
    void_16_params.push_back(const_ptr_10); //Tool
    return void_16_params;
}

std::string LoopRDTSCInstrumentation::createFunctionName(Loop *L, Function *oldFunction) {
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
    std::string Original_location = removeExtension(firstLoc.getFilename().str());
    std::string File = removeExtension(module_name);
    if(File == Original_location)
        newFunctionName = "__invivo__" + File + separator + oldFunction->getName().str() + separator + firstLine;
    else
        newFunctionName = "__invivo__" + File + separator + Original_location + separator + oldFunction->getName().str() + separator + firstLine;
  }
  else {
    newFunctionName = "__invivo__" + oldFunction->getName().str();
  }
  newFunctionName = removeChar(newFunctionName, '-', '_');
  newFunctionName = removeChar(newFunctionName, '/', '_');
  newFunctionName = removeChar(newFunctionName, '+', '_');
  newFunctionName = removeChar(newFunctionName, '.', '_');
  return newFunctionName;
}

GlobalVariable* create_invocation_counter(Module *mod)
{
    GlobalVariable* gvar_int32_count = new GlobalVariable(/*Module=*/*mod, 
    /*Type=*/IntegerType::get(mod->getContext(), 32),
    /*isConstant=*/false,
    /*Linkage=*/GlobalValue::InternalLinkage,
    /*Initializer=*/0, // has initializer, specified below
    /*Name=*/"cere_invocation_counter");
    gvar_int32_count->setAlignment(4);
    ConstantInt* const_int32_0 = ConstantInt::get(mod->getContext(), APInt(32, StringRef("0"), 10));
    gvar_int32_count->setInitializer(const_int32_0);
    return gvar_int32_count;
}

bool LoopRDTSCInstrumentation::runOnFunction(Function &F)
{
    Module* mod = F.getParent();
    if(Mode == VIVO) { //Not replaying a loop so we have to insert init in main function
        std::vector<Type*>FuncTy_8_args;

        PointerType* PointerTy_0 = PointerType::get(IntegerType::get(mod->getContext(), 8), 0);
        FuncTy_8_args.push_back(PointerTy_0);
        FunctionType* FuncTy_8 = FunctionType::get(
                        /*Result=*/Type::getVoidTy(mod->getContext()),
                        /*Params=*/FuncTy_8_args,
                        /*isVarArg=*/false);

        // No main, no instrumentation!
        Function *Main = mod->getFunction("main");

        // Using fortran? ... this kind of works
        if (!Main)
            Main = mod->getFunction("MAIN__");

        if (Main) {
            BasicBlock *firstBB = dyn_cast<BasicBlock>(Main->begin());
            std::vector<BasicBlock*> ReturningBlocks;
            for(Function::iterator I = Main->begin(), E = Main->end(); I != E; ++I) {
                if (isa<ReturnInst>(I->getTerminator())) ReturningBlocks.push_back(I);
            }
            if(measureAppli) {
                FunctionType* FuncTy_0 = createFunctionType(mod);
                std::vector<Value*> funcParameter = createFunctionParameters(mod, "main");
                Function *startFunction = mod->getFunction("cere_markerStartRegion");
                Function *stopFunction = mod->getFunction("cere_markerStopRegion");
                if(!startFunction) {
                    Function* func_start = createFunction(FuncTy_0, mod, "cere_markerStartRegion");
                    CallInst *start = CallInst::Create(func_start, funcParameter, "", &firstBB->front());
                }
                if(!stopFunction) {
                    Function* func_stop = createFunction(FuncTy_0, mod, "cere_markerStopRegion");
                    //We must insert stop probe in all exits blocks
                    for (std::vector<BasicBlock*>::iterator I = ReturningBlocks.begin(), E = ReturningBlocks.end(); I != E; ++I) {
                        CallInst::Create(func_stop, funcParameter, "", &(*I)->back());
                    }
                }
            }
            std::vector<Value*> funcParameter = createInitParameters(mod);
            Function *initFunction = mod->getFunction("cere_markerInit");
            if(!initFunction) {
                Function *initFunction = Function::Create(FuncTy_8,
                                GlobalValue::ExternalLinkage,
                                "cere_markerInit",
                                mod);
                CallInst::Create(initFunction, funcParameter, "", &firstBB->front());
                DEBUG(dbgs() << "Init successfuly inserted in main function\n");
            }
            Function *closeFunction = mod->getFunction("cere_markerClose");
            if(!closeFunction) {
                Function *closeFunction = Function::Create(FuncTy_8,
                                GlobalValue::ExternalLinkage,
                                "cere_markerClose",
                                mod);
                for (std::vector<BasicBlock*>::iterator I = ReturningBlocks.begin(), E = ReturningBlocks.end(); I != E; ++I) {
                    CallInst::Create(closeFunction, funcParameter, "", &(*I)->back());
                }
                DEBUG(dbgs() << "Close successfuly inserted in main function\n");
            }
        }
    }
    if(!measureAppli) {
        LoopInfo &LI = getAnalysis<LoopInfo>();
        std::vector<Loop*> SubLoops(LI.begin(), LI.end());
        for (unsigned i = 0, e = SubLoops.size(); i != e; ++i)
            visitLoop(SubLoops[i], mod);
    }
    return true;
}

bool LoopRDTSCInstrumentation::visitLoop(Loop *L, Module *mod)
{
    //get current module
    Function* currFunc = L->getHeader()->getParent();

    if(L->getLoopDepth() > 1) return false; //ensure we are working only on outermost loops
    if(!L->isLoopSimplifyForm()) {
        DEBUG(dbgs() << "Loop found, but not in simple form...\n");
        return false;
    }

    std::string newFunctionName;
    std::size_t found = currFunc->getName().find("__extracted__");
    if (found == std::string::npos) newFunctionName = createFunctionName(L, currFunc);
    else newFunctionName = currFunc->getName();

    //We are in replay mode, check if current loop is the loop we want to isntrument
    if (Loopname != "all" && Loopname != newFunctionName) {
        return false;
    }
    else if(readFromFile) {
        bool found=false;
        for(std::vector<std::string>::iterator it = loopsToInstrument.begin(); it != loopsToInstrument.end(); it++)
        {
            std::size_t pos = it->find(newFunctionName);
            if (pos != std::string::npos) {
                found=true;
                break;
            }
        }
        if(!found) return false;
    }

    DEBUG(dbgs() << "Loop to instrument " << newFunctionName << "\n");
    errs() << "The invocation to measure is " << invoc << "\n";

    ++LoopCounter;
    //Create start and stop function
    FunctionType* FuncTy_0 = createFunctionType(mod);
    Function* func_start = createFunction(FuncTy_0, mod, "cere_markerStartRegion");
    Function* func_stop = createFunction(FuncTy_0, mod, "cere_markerStopRegion");

    //Get successor and predecessor loop basic block
    BasicBlock *PredBB = L->getLoopPredecessor();
    SmallVector<BasicBlock*,8> exitblocks;
    L->getExitBlocks(exitblocks);
    if(PredBB == NULL || exitblocks.size() == 0)
    {
        std::cerr << "Loop Preds or Succs not found\n";
        DEBUG(dbgs() << "Loop Preds or Succs not found\n");
        return false;
    }
    std::vector<Value*> funcParameter;
    if(invoc != 0)
    {
        //Create invocation counter
        GlobalVariable* gvar_int32_count = create_invocation_counter(mod);

        LoadInst* int32_38 = new LoadInst(gvar_int32_count, "", false, &PredBB->back());
        int32_38->setAlignment(4);
        ConstantInt* const_int32_1 = ConstantInt::get(mod->getContext(), APInt(32, StringRef("1"), 10));
        BinaryOperator* int32_inc = BinaryOperator::Create(Instruction::Add, int32_38, const_int32_1, "inc", &PredBB->back());
        StoreInst* void_39 = new StoreInst(int32_inc, gvar_int32_count, false, &PredBB->back());
        void_39->setAlignment(4);
        LoadInst* int32_39 = new LoadInst(gvar_int32_count, "", false, &PredBB->back());
        int32_39->setAlignment(4);

        //Create function parameter
        funcParameter = createFunctionParameters(mod, newFunctionName, int32_39);
    }
    else {
        funcParameter = createFunctionParameters(mod, newFunctionName);
    }
    CallInst::Create(func_start, funcParameter, "", &PredBB->back());
    for (SmallVectorImpl<BasicBlock *>::iterator I = exitblocks.begin(), E = exitblocks.end(); I != E; ++I)
    {
        CallInst::Create(func_stop, funcParameter, "", &((*I)->back()));
    }
    return true;
}
