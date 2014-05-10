cmake_minimum_required(VERSION 2.8)


set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)
find_package(Clang REQUIRED)

include_directories(${CLANG_INCLUDE_DIRS})
add_definitions(${CLANG_DEFINITIONS})

add_subdirectory(LoopExtractorAll)
add_subdirectory(LoopInstrumentation)
add_subdirectory(LoopManager)
