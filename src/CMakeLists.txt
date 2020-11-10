add_executable(coxianc main.cpp)
add_subdirectory(parse)
add_subdirectory(message)
add_subdirectory(lex)
add_subdirectory(visit)
add_subdirectory(codegen)
add_subdirectory(typing)
add_subdirectory(replicate)

# Also (mostly) copied and pasted and shifted around a bit from LLVM Docs - Building LLVM with CMake - Embedding LLVM in your project
# https://releases.llvm.org/3.6.0/docs/CMake.html#embedding-llvm-in-your-project
target_include_directories(coxianc PRIVATE "../include" ${LLVM_INCLUDE_DIRS})
# end copy and paste

# this is mostly from https://stackoverflow.com/questions/62926242/how-to-link-all-llvm-libraries-in-cmake
# because my personal box only has libLLVM.so installed, and so llvm_map_components_to_libnames errors out, saying "Target X86 is not in the set of libraries."
execute_process(COMMAND llvm-config --libs all
                OUTPUT_VARIABLE llvm_libs
                RESULT_VARIABLE llvm_config_result
                OUTPUT_STRIP_TRAILING_WHITESPACE)

if (NOT ${llvm_config_result} EQUAL 0)
    message(FATAL_ERROR "llvm-config errored with exit code ${llvm_config_result}")
endif()

message(STATUS "Found libraries from llvm-config: ${llvm_libs}")
target_link_libraries(coxianc ${llvm_libs})

set_target_properties(coxianc PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin")

target_compile_options(coxianc PRIVATE -Wall -Wextra -pedantic -pg -static)
