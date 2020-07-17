cmake_minimum_required(VERSION 3.17.3)

project(COxian
    DESCRIPTION "The C++ LLVM-Based Compiler Implementation of the Oxian Language"    
    LANGUAGES CXX)
    
set(CMAKE_CXX_STANDARD 17)

add_subdirectory(src)
