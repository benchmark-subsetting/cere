#include_directories(${LLVM_INCLUDE_DIRS})
#add_definitions(${LLVM_DEFINITIONS})

add_library(LoopInstrumentation SHARED
    LoopInstrumentation.cpp)
