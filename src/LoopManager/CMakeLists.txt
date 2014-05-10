#include_directories(${LLVM_INCLUDE_DIRS})
#add_definitions(${LLVM_DEFINITIONS})

add_library(LoopManager SHARED
    LoopManager.cpp)
