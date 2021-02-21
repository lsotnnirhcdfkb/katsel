add_executable(katselc main.cpp)
add_subdirectory(parse)
add_subdirectory(message)
add_subdirectory(lex)
add_subdirectory(ir)
add_subdirectory(codegen)
add_subdirectory(mangle)
add_subdirectory(lower)
add_subdirectory(ast)
add_subdirectory(utils)

# Also (mostly) copied and pasted and shifted around a bit from LLVM Docs - Building LLVM with CMake - Embedding LLVM in your project
# https://releases.llvm.org/3.6.0/docs/CMake.html#embedding-llvm-in-your-project
target_include_directories(katselc PRIVATE "../include" ${LLVM_INCLUDE_DIRS})
# end copy and paste

# this is mostly from https://stackoverflow.com/questions/62926242/how-to-link-all-llvm-libraries-in-cmake
execute_process(COMMAND llvm-config --libs all
                OUTPUT_VARIABLE llvm_libs
                RESULT_VARIABLE llvm_config_result
                OUTPUT_STRIP_TRAILING_WHITESPACE)

if (NOT ${llvm_config_result} EQUAL 0)
    message(FATAL_ERROR "llvm-config errored with exit code ${llvm_config_result}")
endif()

message(STATUS "Found libraries from llvm-config: ${llvm_libs}")
target_link_libraries(katselc ${llvm_libs})

set_target_properties(katselc PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin"
    CXX_STANDARD 17
    CXX_STANDARD_REQUIRED YES
    CXX_EXTENSIONS NO)

target_compile_options(katselc PRIVATE -Wall -Wextra -Wpedantic)
