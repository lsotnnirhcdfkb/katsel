cmake_minimum_required(VERSION 3.17.3)

project(Katsel
    DESCRIPTION "The Katsel compiler"
    LANGUAGES CXX
    VERSION 0.0.0)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED YES)
set(CMAKE_CXX_EXTENSIONS NO)

option(GENDOC "Build documentation" ON)

# Copied and pasted and shifted around a bit from LLVM Docs - Building LLVM with CMake - Embedding LLVM in your project
# https://releases.llvm.org/3.6.0/docs/CMake.html#embedding-llvm-in-your-project
# find_package(LLVM REQUIRED CONFIG)
# message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
# message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

# add_definitions(${LLVM_DEFINITIONS})
# end copy and paste

find_package(Python REQUIRED COMPONENTS Interpreter)

set(GENCODEALLPY ${CMAKE_SOURCE_DIR}/utils/genall.py)
add_custom_target(gencode ALL
    COMMAND ${Python_EXECUTABLE} ${GENCODEALLPY}
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
    COMMENT "Generate code into .h and .cpp files"
    VERBATIM)

add_subdirectory(src)
add_subdirectory(docs)
add_subdirectory(test)
