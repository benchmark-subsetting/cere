#define DEBUG_TYPE "loop-instrumentation"
#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/LoopPass.h"
#include "llvm/Analysis/LoopInfo.h"
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Type.h>
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/IR/Module.h"
#include "llvm/DebugInfo.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/CommandLine.h"
#include <fstream>
#include <sstream>
#include <iostream>

using namespace llvm;
enum{VIVO, VITRO};

STATISTIC(LoopCounter, "Counts number of loops instrumented");

static cl::opt<std::string>
IsolateLoop("instrument-loop", cl::init("all"),
                  cl::value_desc("loopname"),
                  cl::desc("loop-instrument will only instrument this loop"));
static cl::opt<std::string>
LoopsFilename("loops-file", cl::init(""),
                  cl::value_desc("filename"),
                  cl::desc("File with loops to instrument"));
static cl::opt<std::string>
TraceLoop("loop-to-trace", cl::init(""),
                  cl::value_desc("trace"),
                  cl::desc("instrumentation-mode allow to trace and record all invocations measures for this loop"));
static cl::opt<bool>
NestedInstrumentation("nested-loops", cl::init(true),
                  cl::value_desc("Boolean"),
                  cl::desc("This options allow/deactivate measures of nested loops"));

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
    std::string Loopfile;
    std::string LoopToTrace;
    int Mode;
    bool nestedIsAllowed;
    bool readFromFile;
    std::vector<std::string> loopsToInstrument;
    LoopInfo *LI;  // The current loop information

    explicit LoopRDTSCInstrumentation(int mode = VITRO, unsigned numLoops = ~0, const std::string &loopname = "")
    : FunctionPass(ID), NumLoops(numLoops), Loopname(loopname), Loopfile(LoopsFilename), LoopToTrace(TraceLoop), Mode(mode), nestedIsAllowed(NestedInstrumentation) {
        if (loopname.empty()) Loopname = IsolateLoop;
        if (Loopfile.empty()) readFromFile = false;
        else {
            std::string line;
            std::ifstream loopstream(Loopfile.c_str(), std::ios::in);
            if(loopstream.is_open()) {
                while(getline(loopstream, line)) {
                    line = removeChar(line, '#', ' ');
                    loopsToInstrument.push_back(line);
                    //~ errs() << "line = " << line << "\n";
                }
                readFromFile = true;
            }
            else {
                errs() << "Cannot open file >" << Loopfile << "<\n";
                readFromFile = false;
            }
        }
    }

    virtual bool runOnFunction(Function &F);
    bool checkParameters();
    bool visitLoop(Loop *L, Module *mod);
    std::vector<Value*> createFunctionParameters(Module* mod, std::string newFunctionName);

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
 
    std::vector<Type*>FuncTy_6_args;
    FuncTy_6_args.push_back(PointerTy_4);
    FuncTy_6_args.push_back(IntegerType::get(mod->getContext(), 1));
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
    AttributeSet tmp_PAL;
    {
        SmallVector<AttributeSet, 4> Attrs;
        AttributeSet PAS;
        {
            AttrBuilder B;
            B.addAttribute(Attribute::ZExt);
            PAS = AttributeSet::get(mod->getContext(), 2U, B);
        }
        Attrs.push_back(PAS);
        {
            AttrBuilder B;
            B.addAttribute(Attribute::NoUnwind);
            B.addAttribute(Attribute::UWTable);
            PAS = AttributeSet::get(mod->getContext(), ~0U, B);
        }
       Attrs.push_back(PAS);
       tmp_PAL = AttributeSet::get(mod->getContext(), Attrs);
    }
    tmp->setAttributes(tmp_PAL);
    return tmp;
}

std::vector<Value*> LoopRDTSCInstrumentation::createFunctionParameters(Module* mod, std::string newFunctionName)
{
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

    ConstantInt* const_int1_11;
    if(newFunctionName == LoopToTrace) const_int1_11 = ConstantInt::get(mod->getContext(), APInt(1, StringRef("-1"), 10));
    else const_int1_11 = ConstantInt::get(mod->getContext(), APInt(1, StringRef("0"), 10));
    
    // Global Variable Definitions
    gvar_array__str->setInitializer(param_name);
    std::vector<Value*> void_16_params;
    void_16_params.push_back(const_ptr_11);
    void_16_params.push_back(const_int1_11);
    return void_16_params;
}

//Uncomment if you want also loop last
//line in the isolated function name (does not work very well)
std::string createFunctionName(Loop *L, Function *oldFunction) {
  std::string newFunctionName;
  std::ostringstream oss;
  BasicBlock *firstBB = L->getBlocks()[0];
  //~ BasicBlock *lastBB = Blocks[Blocks.size()-1];
  if (MDNode *firstN = firstBB->front().getMetadata("dbg")) {
    //~ MDNode *lastN = lastBB->back().getMetadata("dbg");
    DILocation firstLoc(firstN);
    //~ DILocation lastLoc(lastN);
    oss << firstLoc.getLineNumber();
    std::string firstLine = oss.str();
    //~ oss.clear();
    //~ oss << lastLoc.getLineNumber();
    //~ std::string lastLine = oss.str();
    std::string File = removeExtension(firstLoc.getFilename().str());
    newFunctionName = "__invivo__" + File + "_" + oldFunction->getName().str() + "_" + firstLine;// + "_" + lastLine;
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

bool LoopRDTSCInstrumentation::checkParameters()
{
    if(Mode == VITRO && (Loopname == "bench" || Loopname == "all")) {
        errs() << "Incompatible parameters, Mode = "<< Mode << " and Loopname = " << Loopname << "!\n";
        return false;
    }
    if(Loopname != "all" && readFromFile) {
        errs() << "Incompatible parameters, can not instrument a particular loop and read from a file!\n";
        return false;
    }
    return true;
}

bool LoopRDTSCInstrumentation::runOnFunction(Function &F)
{
    if(!checkParameters()) return false;

    Module* mod = F.getParent();
    if(Mode == VIVO) { //Not replaying a loop so we have to insert init in main function
        std::vector<Type*>FuncTy_8_args;
        FuncTy_8_args.push_back(IntegerType::get(mod->getContext(), 1));
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
            
            if(Loopname == "bench") {
                FunctionType* FuncTy_0 = createFunctionType(mod);
                std::vector<Value*> funcParameter = createFunctionParameters(mod, "main");
                Function *startFunction = mod->getFunction("rdtsc_markerStartRegion");
                Function *stopFunction = mod->getFunction("rdtsc_markerStopRegion");
                if(!startFunction) {
                    Function* func_start = createFunction(FuncTy_0, mod, "rdtsc_markerStartRegion");
                    CallInst::Create(func_start, funcParameter, "", &firstBB->front());
                }
                if(!stopFunction) {
                    Function* func_stop = createFunction(FuncTy_0, mod, "rdtsc_markerStopRegion");
                    //We must insert stop probe in all exits blocks
                    for (std::vector<BasicBlock*>::iterator I = ReturningBlocks.begin(), E = ReturningBlocks.end(); I != E; ++I) {
                        CallInst::Create(func_stop, funcParameter, "", &(*I)->back());
                    }
                }
            }
            Function *initFunction = mod->getFunction("likwid_markerInit");
            if(!initFunction) {
                Function *initFunction = Function::Create(FuncTy_8,
                                GlobalValue::ExternalLinkage,
                                "likwid_markerInit",
                                mod);
                //Create parameters for init function
                std::vector<Value*> initParameter;
                ConstantInt* const_int;
                if(nestedIsAllowed) const_int = ConstantInt::get(mod->getContext(), APInt(1, StringRef("-1"), 10));
                else const_int = ConstantInt::get(mod->getContext(), APInt(1, StringRef("0"), 10));
                initParameter.push_back(const_int);
                CallInst::Create(initFunction, initParameter, "", &firstBB->front());
                DEBUG(dbgs() << "Init successfuly inserted in main function\n");
            }
        }
    }
    if(Loopname != "bench") {
        LoopInfo &LI = getAnalysis<LoopInfo>();
        std::vector<Loop*> SubLoops(LI.begin(), LI.end());
        for (unsigned i = 0, e = SubLoops.size(); i != e; ++i)
            visitLoop(SubLoops[i], mod);
    }
    return true;
}

/**TODO:
 * 1) Place in main a call to init_rdtsc and close_rdtsc
 * 2) If LoopExtractorAll has been called, we instrument isolated loops
 *  otherwise we are dealing with invivo loops.
**/ 
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
        //~ if(!(std::find(loopsToInstrument.begin(), loopsToInstrument.end(), newFunctionName) != loopsToInstrument.end()))
        //~ return 0;
    }
    //This is hardcoded because when instrumenting the stl deque function, we got segfaults.
    std::size_t found1 = newFunctionName.find("stl");
    std::size_t found2 = newFunctionName.find("deque");
    if (found1 != std::string::npos || found2 != std::string::npos)
    {
        DEBUG(dbgs() << "Illegal instrumentation :" << newFunctionName << "\n");
        return 0;
    }

    DEBUG(dbgs() << "Loop to instrument " << newFunctionName << "\n");

    //Create start and stop function
    FunctionType* FuncTy_0 = createFunctionType(mod);
    Function* func_start = createFunction(FuncTy_0, mod, "rdtsc_markerStartRegion");
    Function* func_stop = createFunction(FuncTy_0, mod, "rdtsc_markerStopRegion");

    //Get successor and predecessor loop basic block
    BasicBlock *PredBB = L->getLoopPredecessor();
    BasicBlock *SuccBB = L->getExitBlock();
    
    if(PredBB == NULL || SuccBB == NULL)
    {
        DEBUG(dbgs() << "Loop Preds or Succs not found\n");
        return false;
    }

    ++LoopCounter;
    std::vector<Value*> funcParameter = createFunctionParameters(mod, newFunctionName);

    CallInst::Create(func_start, funcParameter, "", &PredBB->back());
    CallInst::Create(func_stop, funcParameter, "", &SuccBB->front());

    return true;
}
