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
static cl::opt<bool>
TraceLoop("trace", cl::init(false),
                  cl::value_desc("Boolean"),
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
    bool LoopToTrace;
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
    std::vector<Value*> createFunctionParameters(Module* mod, std::string newFunctionName);
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
    FunctionType* tmp = FunctionType::get(
    /*Result=*/Type::getVoidTy(mod->getContext()),
    /*Params=*/FuncTy_6_args,
    /*isVarArg=*/false);
    return tmp;
}


void print_api(Module* mod, BasicBlock *BB, LoadInst* int32_42)
{
    //~ LoadInst* int32_42 = new LoadInst(gvar_int32_count, "", false, BB);
    //~ int32_42->setAlignment(4);
    Constant *const_array_18 = ConstantDataArray::getString(mod->getContext(), "COUNT = %d\x0A", true);
    ArrayType* ArrayTy_5 = ArrayType::get(IntegerType::get(mod->getContext(), 8), 12);
    GlobalVariable* gvar_array__str3 = new GlobalVariable(/*Module=*/*mod, 
    /*Type=*/ArrayTy_5,
    /*isConstant=*/true,
    /*Linkage=*/GlobalValue::PrivateLinkage,
    /*Initializer=*/0, // has initializer, specified below
    /*Name=*/".str3");
    gvar_array__str3->setAlignment(1);
    ConstantInt* const_int32_20 = ConstantInt::get(mod->getContext(), APInt(32, StringRef("0"), 10));
    std::vector<Constant*> const_ptr_24_indices;
    const_ptr_24_indices.push_back(const_int32_20);
    const_ptr_24_indices.push_back(const_int32_20);
    Constant* const_ptr_24 = ConstantExpr::getGetElementPtr(gvar_array__str3, const_ptr_24_indices);
    gvar_array__str3->setInitializer(const_array_18);
    std::vector<Value*> int32_call_43_params;
    int32_call_43_params.push_back(const_ptr_24);
    int32_call_43_params.push_back(int32_42);
    PointerType* PointerTy_8 = PointerType::get(IntegerType::get(mod->getContext(), 8), 0);
    std::vector<Type*>FuncTy_10_args;
    FuncTy_10_args.push_back(PointerTy_8);
    FunctionType* FuncTy_10 = FunctionType::get(
                        /*Result=*/IntegerType::get(mod->getContext(), 32),
                        /*Params=*/FuncTy_10_args,
                        /*isVarArg=*/true);
    Function* func_printf = mod->getFunction("printf");
    if (!func_printf) {
        func_printf = Function::Create(
        /*Type=*/FuncTy_10,
        /*Linkage=*/GlobalValue::ExternalLinkage,
        /*Name=*/"printf", mod); // (external, no body)
        func_printf->setCallingConv(CallingConv::C);
    }
    CallInst* int32_call_43 = CallInst::Create(func_printf, int32_call_43_params, "call", BB);
    int32_call_43->setCallingConv(CallingConv::C);
    int32_call_43->setTailCall(false);
    AttributeSet int32_call_43_PAL;
    int32_call_43->setAttributes(int32_call_43_PAL);
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
    //~ AttributeSet tmp_PAL;
    //~ {
        //~ SmallVector<AttributeSet, 5> Attrs;
        //~ AttributeSet PAS;
        //~ {
            //~ AttrBuilder B;
            //~ B.addAttribute(Attribute::ZExt);
            //~ PAS = AttributeSet::get(mod->getContext(), 2U, B);
        //~ }
        //~ Attrs.push_back(PAS);
        //~ {
            //~ AttrBuilder B;
            //~ B.addAttribute(Attribute::NoUnwind);
            //~ B.addAttribute(Attribute::UWTable);
            //~ PAS = AttributeSet::get(mod->getContext(), ~0U, B);
        //~ }
       //~ Attrs.push_back(PAS);
       //~ tmp_PAL = AttributeSet::get(mod->getContext(), Attrs);
    //~ }
    //~ tmp->setAttributes(tmp_PAL);
    return tmp;
}

std::vector<Value*> LoopRDTSCInstrumentation::createFunctionParameters(Module* mod, std::string newFunctionName)
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
    if(LoopToTrace) const_int1_11 = ConstantInt::get(mod->getContext(), APInt(1, StringRef("-1"), 10));
    else const_int1_11 = ConstantInt::get(mod->getContext(), APInt(1, StringRef("0"), 10));

    //Set global boolean
    ConstantInt* const_int;
    const_int = ConstantInt::get(mod->getContext(), APInt(1, StringRef("0"), 10)); //always false

    // Global Variable Definitions
    gvar_array__str->setInitializer(param_name);
    gvar_array__str_tool->setInitializer(param_name_tool);
    std::vector<Value*> void_16_params;
    void_16_params.push_back(const_ptr_10); //Tool
    void_16_params.push_back(const_ptr_11); //LoopName
    void_16_params.push_back(const_int1_11); //Trace boolean
    void_16_params.push_back(const_int); //Global measure boolean
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

bool LoopRDTSCInstrumentation::runOnFunction(Function &F)
{
    //~ errs() << "The tool is " << InstruTool << "\n";
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
                    CallInst::Create(func_start, funcParameter, "", &firstBB->front());
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
    //Create function parameter
    std::vector<Value*> funcParameter = createFunctionParameters(mod, newFunctionName);
 
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
    if(invoc != 0) {
        //Create basicBlock where we will insert the marker start
        BasicBlock* label_if_then = BasicBlock::Create(mod->getContext(), "if.then",currFunc,0);

        //Create invocation counter
        GlobalVariable* gvar_int32_count = create_invocation_counter(mod);

        //Store requested invocation
        ConstantInt* const_int32_21 = ConstantInt::get(mod->getContext(), APInt(32, invoc, 10));
        AllocaInst* ptr_invocation = new AllocaInst(IntegerType::get(mod->getContext(), 32), "invocation", &PredBB->back());
        ptr_invocation->setAlignment(4);
        StoreInst* void_34 = new StoreInst(const_int32_21, ptr_invocation, false, &PredBB->back());
        void_34->setAlignment(4);

        //Load requested invocation and the current invocation and compare them
        LoadInst* int32_34 = new LoadInst(ptr_invocation, "", false, &PredBB->back());
        int32_34->setAlignment(4);
        LoadInst* int32_38 = new LoadInst(gvar_int32_count, "", false, &PredBB->back());
        int32_38->setAlignment(4);
        ConstantInt* const_int32_1 = ConstantInt::get(mod->getContext(), APInt(32, StringRef("1"), 10));
        BinaryOperator* int32_inc = BinaryOperator::Create(Instruction::Add, int32_38, const_int32_1, "inc", &PredBB->back());
        StoreInst* void_39 = new StoreInst(int32_inc, gvar_int32_count, false, &PredBB->back());
        void_39->setAlignment(4);

        //Get the entry point of the loop
        TerminatorInst *term = PredBB->getTerminator();
        BasicBlock *succ = term->getSuccessor(0);

        //If requested invocation equal the current invocation, jump to the marker start
        //otherwise, just go to the loop
        ICmpInst* int1_cmp = new ICmpInst(&PredBB->back(), ICmpInst::ICMP_EQ, int32_38, int32_34, "cmp");
        BranchInst::Create(label_if_then, succ, int1_cmp, &PredBB->back());

        //Remove old terminator instruction as we added our own branch condition
        term->eraseFromParent();

        // Block if.then (label_if_then)
        //Call the marker start
        CallInst::Create(func_start, funcParameter, "", label_if_then);
        //Go to the loop
        BranchInst::Create(succ, label_if_then);

        //Update phinode of successor
        if (isa<PHINode>(&succ->front()))
        {
            dyn_cast<PHINode>(&succ->front())->addIncoming( dyn_cast<PHINode>(&succ->front())->getIncomingValueForBlock(PredBB), label_if_then);
        }

        /*Also compare at the end of the loop, invocation and count to call the marker stop and 
         * add the correct return instruction. Do it for each successor of the loop.
        */
        for (SmallVectorImpl<BasicBlock *>::iterator I = exitblocks.begin(), E = exitblocks.end(); I != E; ++I)
        {
            BasicBlock* label_if_else4 = BasicBlock::Create(mod->getContext(), "if.else",currFunc,0);
            BasicBlock* label_if_then4 = BasicBlock::Create(mod->getContext(), "if.then",currFunc,0);

            TerminatorInst *exitTerm = (*I)->getTerminator();
            Instruction *test = exitTerm->clone();

            label_if_else4->getInstList().push_back(test);
            CallInst::Create(func_stop, funcParameter, "", label_if_then4);
            Instruction *test2 = exitTerm->clone();
            label_if_then4->getInstList().push_back(test2);

            ICmpInst* int1_cmp3 = new ICmpInst(&((*I)->back()), ICmpInst::ICMP_EQ, int32_38, int32_34, "cmp3");
            BranchInst::Create(label_if_then4, label_if_else4, int1_cmp3, &((*I)->back()));
            exitTerm->eraseFromParent();
        }
    }
    else {
        CallInst::Create(func_start, funcParameter, "", &PredBB->back());
        for (SmallVectorImpl<BasicBlock *>::iterator I = exitblocks.begin(), E = exitblocks.end(); I != E; ++I)
        {
            CallInst::Create(func_stop, funcParameter, "", &((*I)->back()));
        }
    }

    return true;
}
