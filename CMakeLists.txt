cmake_minimum_required(VERSION 3.17.3)

project(COxian
    DESCRIPTION "The C++ LLVM-Based Compiler Implementation of the Oxian Language"    
    LANGUAGES CXX)
    
set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED YES)
set(CMAKE_CXX_EXTENSIONS NO)

# Copied and pasted and shifted around a bit from LLVM Docs - Building LLVM with CMake - Embedding LLVM in your project
# https://releases.llvm.org/3.6.0/docs/CMake.html#embedding-llvm-in-your-project
find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

add_definitions(${LLVM_DEFINITIONS})
# end copy and paste

add_subdirectory(src)
