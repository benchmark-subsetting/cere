#define DEBUG_TYPE "loop-memory-dump"
#include "llvm/Support/Debug.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/LoopPass.h"
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Type.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/DataLayout.h>
#include "llvm/ADT/SetVector.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/Support/CommandLine.h"
#include <string>
#include <sstream>
#include <fstream>
#include <set>

using namespace llvm;
enum{DUMP, REPLAY};

STATISTIC(LoopCounter, "Counts number of loops dumped");

static cl::opt<std::string>
LoopToDump("loop-to-dump", cl::init("all"),
                  cl::value_desc("loopname"),
                  cl::desc("loop-dump will only dump this loop"));
static cl::opt<std::string>
LoopsFilename("loops-file", cl::init(""),
                  cl::value_desc("filename"),
                  cl::desc("File with loops to dump"));
static cl::opt<int>
Invocation("invocation", cl::init(1),
                  cl::value_desc("Integer"),
                  cl::desc("Allow to dump or replay the n loop execution"));

namespace {
  struct LoopManager : public FunctionPass {
    static char ID;
    int Mode;
    unsigned NumLoops;
    std::string DumpLoop;
    std::string Loopfile;
    int Nloop;
    bool readFromFile;
    bool globalDump;
    std::vector<std::string> loopsToDump;

    explicit LoopManager(int mode = DUMP, unsigned numLoops = ~0)
    : FunctionPass(ID), Mode(mode), NumLoops(numLoops), DumpLoop(LoopToDump), Loopfile(LoopsFilename), Nloop(Invocation) {
        if (DumpLoop == "all") globalDump = true;
        else globalDump = false;
        if (Loopfile.empty()) readFromFile = false;
        else {
            std::string line;
            std::ifstream loopstream(Loopfile.c_str(), std::ios::in);
            if(loopstream.is_open()) {
                while(getline(loopstream,line))
                    loopsToDump.push_back(line);
                readFromFile = true;
            }
            else readFromFile = false;
        }
    }

    virtual bool runOnFunction(Function &F);
    bool visitLoop(Loop *L, Module *mod);

    virtual void getAnalysisUsage(AnalysisUsage &AU) const {
        AU.addRequired<LoopInfo>();
        AU.addPreserved<LoopInfo>();
    }
  };
}

char LoopManager::ID = 0;
static RegisterPass<LoopManager> X("loop-dump", "Dump loops");

namespace {
    struct LoopReplay: public LoopManager {
        static char ID;
        LoopReplay() : LoopManager(REPLAY) {}
    };
}

char LoopReplay::ID = 0;
static RegisterPass<LoopReplay> Y("loop-replay", "Replay loops");

/**Create signature for the dump function:
 **void dump(char*, int, int)
**/
FunctionType* createDumpFunctionType(Module* mod)
{
    PointerType* PointerTy_0 = PointerType::get(IntegerType::get(mod->getContext(), 8), 0);

    std::vector<Type*> FuncTy_args;
    FuncTy_args.push_back(PointerTy_0); //pointer for loop name
    FuncTy_args.push_back(IntegerType::get(mod->getContext(), 32)); //int for the invocation to dump
    //~ FuncTy_args.push_back(IntegerType::get(mod->getContext(), 32)); //int for the number of variable args
    FunctionType* tmp = FunctionType::get(
        /*Result=*/Type::getVoidTy(mod->getContext()),
        /*Params=*/FuncTy_args,
        /*isVarArg=*/true);
    return tmp;
}

/**Create signature for the dump function:
 **void dump(char*, int, int)
**/
FunctionType* createLoadFunctionType(Module* mod)
{
    PointerType* PointerTy_4 = PointerType::get(IntegerType::get(mod->getContext(), 8), 0);
    PointerType* PointerTy_3 = PointerType::get(PointerTy_4, 0);

    std::vector<Type*>FuncTy_10_args;
    FuncTy_10_args.push_back(PointerTy_4); //pointer for loop name
    FuncTy_10_args.push_back(IntegerType::get(mod->getContext(), 32)); //int for the invocation to dump
    FuncTy_10_args.push_back(IntegerType::get(mod->getContext(), 32)); //int for the number of variable args
    FuncTy_10_args.push_back(PointerTy_3); //pointer for the pointer array
    FunctionType* tmp = FunctionType::get(
    /*Result=*/Type::getVoidTy(mod->getContext()),
    /*Params=*/FuncTy_10_args,
    /*isVarArg=*/false);
    return tmp;
}

/**Create a pointer to string s**/
Constant* createStringValue(Module* mod, std::string &s) {
    Constant *const_array_0 = ConstantDataArray::getString(mod->getContext(), s, true);
    GlobalVariable* gvar_array__str = new GlobalVariable(/*Module=*/*mod,
    /*Type=*/const_array_0->getType(),
    /*isConstant=*/true,
    /*Linkage=*/GlobalValue::PrivateLinkage,
    /*Initializer=*/0, // has initializer, specified below
    /*Name=*/".str");
    gvar_array__str->setAlignment(1);
    ConstantInt* const_int32_0 = ConstantInt::get(mod->getContext(), APInt(32, StringRef("0"), 10));
    std::vector<Constant*> const_ptr_0_indices;
    const_ptr_0_indices.push_back(const_int32_0);
    const_ptr_0_indices.push_back(const_int32_0);
    gvar_array__str->setInitializer(const_array_0);
    return ConstantExpr::getGetElementPtr(gvar_array__str, const_ptr_0_indices);
}

/**Creates parameters for the dump function:
 * char* loopName
 * int InvocationToDump
 * int number of variable arguments
 * ... variables used by the original loop
**/
std::vector<Value*> createDumpFunctionParameters(Module* mod, Function* currFunc, BasicBlock* PredBB, int N)
{
    //This function copies parameters from the function containing the current loop
    std::vector<Value*> params;
    //~ errs() << "Function: " << currFunc->getName() << "\n";
    for (Function::arg_iterator args = currFunc->arg_begin(); args != currFunc->arg_end(); args++)
    {
        if(args->getType()->isPointerTy() == false) //If argument is a value get its address
        {
            AllocaInst* ptr_args = new AllocaInst(args->getType(), args->getName(), &PredBB->back());
            /*StoreInst* void_0 = */new StoreInst(args, ptr_args, false, &PredBB->back());
            if((ptr_args->getName().str()).empty()) {
                std::ostringstream ss;
                ss << params.size();
                ptr_args->setName(ss.str()+".addr");
            }
            params.push_back(ptr_args);
            //~ std::string type_str;
            //~ llvm::raw_string_ostream rso(type_str);
            //~ ptr_args->getType()->print(rso);
            //~ params.push_back(createStringValue(mod, rso.str()));
            //~ std::string tmp = ptr_args->getName().str();
            //~ params.push_back(createStringValue(mod, tmp));
            //~ errs() << " Name: " << ptr_args->getName() << ", Type:" << *ptr_args->getType() << "\n";
        }
        else {
            Argument* ptr_args = args;
            if((ptr_args->getName().str()).empty()) {
                std::ostringstream ss;
                ss << params.size();
                ptr_args->setName(ss.str()+".addr");
            }
            params.push_back(ptr_args);
            //~ std::string type_str;
            //~ llvm::raw_string_ostream rso(type_str);
            //~ ptr_args->getType()->print(rso);
            //~ params.push_back(createStringValue(mod, rso.str()));
            //~ std::string tmp = ptr_args->getName().str();
            //~ params.push_back(createStringValue(mod, tmp));
            //~ errs() << " Name: " << args->getName() << ", Type:" << *args->getType() << "\n";
        }
    }
    ConstantInt* nbParam = ConstantInt::get(mod->getContext(), APInt(32, params.size(), 10));
    ConstantInt* iterToDump = ConstantInt::get(mod->getContext(), APInt(32, N, 10));

    /*Convert the invocation to dump as string*/
    std::string tmp = currFunc->getName().str();

    params.insert(params.begin(), nbParam);
    params.insert(params.begin(), iterToDump);
    params.insert(params.begin(), createStringValue(mod, tmp));

    return params;
}

/**Creates parameters for the load function:
 * char* loopName
 * int InvocationToDump
 * int number of variable arguments
 * void** array of adresses filled by the load lib
**/
std::vector<Value*> createLoadFunctionParameters(Module* mod, Function* currFunc, BasicBlock* B, int N)
{
    PointerType* PointerTy_4 = PointerType::get(IntegerType::get(mod->getContext(), 8), 0);
    std::vector<Value*> params;

    /**** Get the name of the loop to be loaded ****/
    Constant *param_name = ConstantDataArray::getString(mod->getContext(), currFunc->getName().str(), true);
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
    // Global Variable Definitions
    gvar_array__str->setInitializer(param_name);

    /* The invocation to load */
    ConstantInt* iterToLoad = ConstantInt::get(mod->getContext(), APInt(32, N, 10));

    /* Get the number of argument we will have to load */
    int nbArgs = 0;
    for (Function::arg_iterator args = currFunc->arg_begin(); args != currFunc->arg_end(); args++) nbArgs++;

    //~ //Allocate a "void* args[nbArgs] array"
    ConstantInt* const_int_nbArgs = ConstantInt::get(mod->getContext(), APInt(32, nbArgs, 10));
    CastInst* int64_0 = new ZExtInst(const_int_nbArgs, IntegerType::get(mod->getContext(), 64), "", B);
    AllocaInst* ptr_vla = new AllocaInst(PointerTy_4, int64_0, "vla", B);

    //Push loop name + invocation to load + number of params + adresses array to load params
    params.push_back(const_ptr_11);
    params.push_back(iterToLoad);
    params.push_back(const_int_nbArgs);
    params.push_back(ptr_vla);

    return params;
}

Function* createWrapperFunction(Function* F, Module* M)
{
    //Create a void type
    std::vector<Type*> FuncTy_8_args;
    FunctionType* FuncTy_8 = FunctionType::get(
                        /*Result=*/Type::getVoidTy(M->getContext()),
                        /*Params=*/FuncTy_8_args,
                        /*isVarArg=*/false);
    //Create the wrapper function
    Function *tmp = Function::Create(FuncTy_8,
                        GlobalValue::ExternalLinkage,
                        "run" + F->getName().str(),
                        M);
    return tmp;
}

std::vector<Value*> createLoopParameters(Function* currFunc, Module* mod, AllocaInst* ptr_vla, BasicBlock* label_entry)
{
    std::vector<Value*> params;
    //get back adresses of loop parameters from the array filled by load
    int i=0;
    for (Function::arg_iterator args = currFunc->arg_begin(); args != currFunc->arg_end(); args++)
    {
        UnaryInstruction *ptr;
        ConstantInt* const_int64_35 = ConstantInt::get(mod->getContext(), APInt(32, i, 10));
        GetElementPtrInst* ptr_arrayidx = GetElementPtrInst::Create(ptr_vla, const_int64_35, "arrayidx", label_entry);
        LoadInst* ptr_69 = new LoadInst(ptr_arrayidx, "", false, label_entry); //get args[i]
        DEBUG(dbgs() << "Casting " << *ptr_69->getType() << " to " << *args->getType() << "\n");
        if(args->getType()->isPointerTy()) {//We are happy
            ptr = new BitCastInst(ptr_69, args->getType(), "", label_entry); //cast
            //if args is a scalar, make a copy and give the copy adress to the func
            PointerType *pointerType = dyn_cast<PointerType>(args->getType());
            Type* elementType = pointerType->getElementType();
            if(!elementType->isArrayTy() && !elementType->isStructTy()) {
                AllocaInst* ptr_a_addr = new AllocaInst(args->getType(), "a", label_entry);
                AllocaInst* ptr_tmp = new AllocaInst(args->getType()->getPointerElementType(), "b", label_entry);
                AllocaInst* ptr_tmp2 = new AllocaInst(args->getType()->getPointerElementType(), "c", label_entry);
                //Store ptr adress
                new StoreInst(ptr, ptr_a_addr, false, label_entry);
                LoadInst* ptr_5 = new LoadInst(ptr_a_addr, "", false, label_entry);
                LoadInst* int32_6 = new LoadInst(ptr_5, "", false, label_entry);
                new StoreInst(int32_6, ptr_tmp, false, label_entry);
                LoadInst* int32_9 = new LoadInst(ptr_tmp, "", false, label_entry);
                new StoreInst(int32_9, ptr_tmp2, false, label_entry);
                ptr_tmp2->setName(args->getName());
                params.push_back(ptr_tmp2);
            }
            else {
                ptr->setName(args->getName());
                params.push_back(ptr);
            }
        }
        else {
            PointerType* PointerTy_3 = PointerType::get(args->getType(), 0);
            CastInst* ptr_70 = new BitCastInst(ptr_69, PointerTy_3, "", label_entry); //cast
            ptr = new LoadInst(ptr_70, "", false, label_entry);
            ptr->setName(args->getName());
            params.push_back(ptr);
        }
        i++;
    }
    return params;
}

/**Called on each function int the current Module**/
bool LoopManager::runOnFunction(Function &F)
{
    Module* mod = F.getParent();

    // On Fortran code instrument on MAIN__ (after libgfortran init)
    // On other code instrument on main
    Function *Main = mod->getFunction("MAIN__");
    if (!Main) {
        Main = mod->getFunction("main");
    }

    if (Main) { //We are in the module with the main function
        std::string funcName;
        ConstantInt* const_int1_11;
        if (Mode == REPLAY) {
            funcName="real_main";
            const_int1_11 = ConstantInt::get(mod->getContext(), APInt(1, StringRef("0"), 10)); //false
        }
        else if (Mode == DUMP) {
            funcName="dump_init";
            if(globalDump) const_int1_11 = ConstantInt::get(mod->getContext(), APInt(1, StringRef("-1"), 10)); //true
            else const_int1_11 = ConstantInt::get(mod->getContext(), APInt(1, StringRef("0"), 10)); //false
        }
        Function *mainFunction = mod->getFunction(funcName);
        if(!mainFunction) {
            //Create signature: void func(int)
            std::vector<Type*> FuncTy_8_args;
            FuncTy_8_args.push_back(IntegerType::get(mod->getContext(), 1));
            FunctionType* FuncTy_8 = FunctionType::get(
                                    /*Result=*/Type::getVoidTy(mod->getContext()),
                                    /*Params=*/FuncTy_8_args,
                                    /*isVarArg=*/false);
            Function *mainFunction = Function::Create(FuncTy_8,
                            GlobalValue::ExternalLinkage,
                            funcName,
                            mod);
            std::vector<Value*> void_16_params;
            void_16_params.push_back(const_int1_11);
            CallInst::Create(mainFunction, void_16_params, "", Main->begin()->begin());
        }
    }
    LoopInfo &LI = getAnalysis<LoopInfo>();
    //Get all loops int the current function and visit them
    std::vector<Loop*> SubLoops(LI.begin(), LI.end());
    for (unsigned i = 0, e = SubLoops.size(); i != e; ++i)
        visitLoop(SubLoops[i], mod);
    return true;
}

bool LoopManager::visitLoop(Loop *L, Module *mod)
{
    //Ensure we are working only on outermost loops
    if(L->getLoopDepth() > 1) return false;

    //Get the function we are in
    Function* currFunc = L->getHeader()->getParent();

    if(Mode == DUMP) {
        //To be sure we are dumping an isolated loop
        std::size_t found = currFunc->getName().find("__extracted__");
        if (found == std::string::npos) return false;

        //If we want to extract a particular loop, look if it's this one
        if(DumpLoop != "all" && DumpLoop != currFunc->getName()) {
            return false;
        }
        else if(readFromFile) {
            if(!(std::find(loopsToDump.begin(), loopsToDump.end(), currFunc->getName()) != loopsToDump.end()))
            return 0;
        }
        //Create dump function
        Function* func_dump = mod->getFunction("dump");
        if (!func_dump) { //if function "dump" not found, creates it
            FunctionType* DumpFuncTy = createDumpFunctionType(mod);
            func_dump = Function::Create(DumpFuncTy,
                                         GlobalValue::ExternalLinkage,
                                         "dump", mod);
        }

        //Create after_dump function
        Function* func_after_dump = mod->getFunction("after_dump");
        if (!func_after_dump) { //if function "after_dump" not found, creates it
            //Create a void type
            std::vector<Type*> FuncTy_12_args;
            FunctionType* AfterDumpFuncTy = FunctionType::get(
                                    /*Result=*/Type::getVoidTy(mod->getContext()),
                                    /*Params=*/FuncTy_12_args,
                                    /*isVarArg=*/false);
            func_after_dump = Function::Create(AfterDumpFuncTy,
                                         GlobalValue::ExternalLinkage,
                                         "after_dump", mod);
        }
        //Get predecessor loop basic block
        BasicBlock *PredBB = L->getLoopPredecessor();
        BasicBlock *SuccBB = L->getExitBlock();

        if(PredBB == NULL || SuccBB == NULL) return false;

        ++LoopCounter;
        std::vector<Value*> funcParameter = createDumpFunctionParameters(mod, currFunc, PredBB, Nloop);

        CallInst::Create(func_dump, funcParameter, "", &PredBB->back());
        CallInst::Create(func_after_dump, "", &SuccBB->front());
    }
    else if (Mode == REPLAY) {
        //To be sure we are creating a wrapper for an isolated loop
        std::size_t found = currFunc->getName().find("__extracted__");
        if (found == std::string::npos) return false;

        /*Create the wrapper function and add a basicBlock in it*/
        Function *wrapperFunction = createWrapperFunction(currFunc, mod);
        BasicBlock* label_entry = BasicBlock::Create(mod->getContext(), "entry", wrapperFunction, 0);

        /*Now we create the load function and add a call in the wrapper function*/
        
        //Create the signature of the load function
        FunctionType* LoadFuncTy = createLoadFunctionType(mod);
        //Create the function load
        Function* func_load = mod->getFunction("load");
        if (!func_load) {
            func_load = Function::Create(
                        /*Type=*/LoadFuncTy,
                        /*Linkage=*/GlobalValue::ExternalLinkage,
                        /*Name=*/"load", mod);
        }
        //Params for the load function
        std::vector<Value*> loadParams = createLoadFunctionParameters(mod, currFunc, label_entry, Nloop);

        //Add call to load
        CallInst::Create(func_load, loadParams, "", label_entry);

        /* Ok now we have adresses of variables used by the loop in ptr_vla array,
         * so we have to read it, and create parameters for the loop */
        std::vector<Value*> loopParams = createLoopParameters(currFunc, mod, cast<AllocaInst>(loadParams.back()), label_entry);

        //Call the function with the extracted loop
        CallInst::Create(currFunc, loopParams, "", label_entry);

        //We must add an anti dead code function
        std::vector<Type*> FuncADCE_args;
        //Signature: void anti_dead_code_elimination(int, ...);
        FuncADCE_args.push_back(IntegerType::get(mod->getContext(), 32));
        FunctionType* FuncADCE_type = FunctionType::get(
            /*Result=*/Type::getVoidTy(mod->getContext()),
            /*Params=*/FuncADCE_args,
            /*isVarArg=*/true);
        //create antiDCE function
        Function *antiDeadCodeFunction = Function::Create(FuncADCE_type,
                            GlobalValue::ExternalLinkage,
                            "anti_dead_code_elimination",
                            mod);
        antiDeadCodeFunction->addFnAttr(Attribute::NoInline);
        /*AntiDCE function takes exactly the same parameters then the loop
         * + the number of parameters as we pass them as variable argument*/
        ConstantInt* nbParam = ConstantInt::get(mod->getContext(), APInt(32, loopParams.size(), 10));
        loopParams.insert(loopParams.begin(), nbParam);
        //Add the call
        CallInst::Create(antiDeadCodeFunction, loopParams, "", label_entry);
        //Add a return instruction for wrapper function
        ReturnInst::Create(mod->getContext(), label_entry);
    }
    return true;
}
