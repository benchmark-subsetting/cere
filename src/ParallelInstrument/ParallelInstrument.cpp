#include "llvm/ADT/Statistic.h"
#include "llvm/Analysis/LoopPass.h"
#include "llvm/Analysis/LoopInfo.h"
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Type.h>
#include "llvm/Transforms/IPO.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/IR/Module.h"
#include "llvm/DebugInfo.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/InstIterator.h"
#include <fstream>
#include <sstream>
#include <iostream>
#include <string> 

    using namespace llvm;

    namespace 
    {
      struct ProbeRDTSCOMP : public FunctionPass 
      {
        bool insertProbe(Module* mod,Function *F);
        bool insertInitProbe(Module* mod,Function *F);
        
        static char ID;
        ProbeRDTSCOMP() : FunctionPass(ID) {}

        virtual void getAnalysisUsage(AnalysisUsage &AU) const 
        {
          AU.addRequiredID(BreakCriticalEdgesID);
          AU.addRequired<LoopInfo>();
          AU.addPreserved<LoopInfo>();
          AU.addRequired<DominatorTree>();
        }

        virtual bool runOnFunction(Function &F) 
        {
            Module* mod = F.getParent();                                    
            
            insertInitProbe(mod,&F);
            insertProbe(mod,&F);                
            
            return false;               
        }
      };
    }

    char ProbeRDTSCOMP::ID = 0;
    static RegisterPass<ProbeRDTSCOMP> X("ParallelInstrument", "automiatically instrument parallel regions", false, false);
    

/**************************************************************************************/

// Create function void fct (char * ) 
FunctionType* createFunctionTypeProbe(Module* mod)
{ 
    //int
    //PointerType* PointerTy_4 = IntegerType::get(mod->getContext(), 32);
    
    //char *
    PointerType* PointerTy_4 = PointerType::get(IntegerType::get(mod->getContext(), 8), 0);
    
    std::vector<Type*>FuncTy_3_args;    
    FuncTy_3_args.push_back(PointerTy_4);
    FuncTy_3_args.push_back(IntegerType::get(mod->getContext(), 1));
    FunctionType* tmp = FunctionType::get(
    /*Result=*/Type::getVoidTy(mod->getContext()),
    /*Params=*/FuncTy_3_args,
    /*isVarArg=*/false);
    return tmp;
} 

// Create function  void fct ( ) 
FunctionType* createFunctionTypeinit(Module* mod)
{
    std::vector<Type*>FuncTy_3_args;    
    FunctionType* tmp = FunctionType::get(
    /*Result=*/Type::getVoidTy(mod->getContext()),
    /*Params=*/FuncTy_3_args,
    /*isVarArg=*/false);
    return tmp;
} 

// Specify if the function call has to be wrapped
bool choosePutMarkerOnFunction(std::string functionCall)
{
    if(functionCall.find("fork") != std::string::npos )
        return true;
    else
        return false;
}

// Create a function if she is not in the current module mod
Function *checkCreateProbeFunction(Module* mod,std::string marker)
{
    Function *probeFunction = mod->getFunction(marker);     
    if (!probeFunction)
    {
        std::vector<Type*>FuncTy_8_args;
        FunctionType* FuncTy_8 = createFunctionTypeProbe(mod);
        Function::Create(FuncTy_8,GlobalValue::ExternalLinkage,marker,mod);
        probeFunction = mod->getFunction(marker); 
    }    
    return probeFunction;
}

// Return function parameters
std::vector<Value*> createFunctionParameters(Module* mod, std::string newFunctionName)
{
    Constant *param_name = ConstantDataArray::getString(mod->getContext(), newFunctionName, true); 
    
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
    const_int1_11 = ConstantInt::get(mod->getContext(), APInt(1, StringRef("0"), 10));
    
    // Global Variable Definitions
    gvar_array__str->setInitializer(param_name);
    std::vector<Value*> void_16_params;
    void_16_params.push_back(const_ptr_11);
    void_16_params.push_back(const_int1_11);
    return void_16_params;
}

// Define marker start stop names for a specific parralel region
std::string defineProbeName(Function *F,CallInst* callInst,Module* mod) 
{
    std::string line;
    line = mod->getModuleIdentifier();
        
    std::string str;
    llvm::raw_string_ostream rso(str);
    callInst->print(rso);

    size_t place = str.find(".omp_microtask");
    if(place == std::string::npos)
        return F->getName();
    
    else    
    {
        size_t place2 = str.substr(place).find(" ");
        return line+"_"+str.substr(place,place2);
    }
}

// Insert current marker to wrapper any call in the function F
void insertMarker(Module* mod,Function *F,std::string marker,int place)
{
    bool insertMarker;
    insertMarker = false;

    std::string functionCall;
    std::string probeName;
    std::vector<Value*> funcParameter;

    Function* probeFunction = checkCreateProbeFunction(mod,marker);                    
        
    for (inst_iterator I = inst_begin(F), E = inst_end(F); I != E; ++I)
    {
        if (insertMarker)
        {
            Instruction* pinst = dyn_cast<Instruction>(&*I);
            CallInst::Create(probeFunction,funcParameter ,"",pinst);
            insertMarker = false;
        }    
        
        if (CallInst* callInst = dyn_cast<CallInst>(&*I))
        {            
            functionCall = callInst->getCalledFunction()->getName();
            probeName = defineProbeName(F,callInst,mod);
            funcParameter = createFunctionParameters(mod,probeName);
            
            if (choosePutMarkerOnFunction(functionCall))
            {
                if (place == 0)
                    CallInst::Create(probeFunction,funcParameter,"",callInst);
                else
                    insertMarker = true;
            }
        }        
    }
}

/**************************************************************************************/

// Insert init probe in main
bool ProbeRDTSCOMP::insertInitProbe(Module* mod,Function *F)
{
    Function *initFunction = mod->getFunction("likwid_markerInit");
    if(!initFunction) 
    {
        Function *Main = mod->getFunction("main");
        if (Main)
        {
            BasicBlock *firstBB = dyn_cast<BasicBlock>(Main->begin());
            FunctionType* FuncTy_8 = createFunctionTypeinit(mod);
            Function *initFunction = Function::Create(FuncTy_8,GlobalValue::ExternalLinkage,"likwid_markerInit",mod);
            CallInst::Create(initFunction, "", &firstBB->front());
        }
    }               
    return true;
}

// Insert rdtsc probe in the current function 
// Except for main    
bool ProbeRDTSCOMP::insertProbe(Module* mod,Function *F)
{     
    insertMarker(mod,F,"rdtsc_markerStartRegion",0);
    insertMarker(mod,F,"rdtsc_markerStopRegion",1);
    return true;
}



