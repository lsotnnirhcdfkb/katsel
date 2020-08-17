add_executable(coxianc main.cpp lexer.cpp parser.cpp errors.cpp ast.cpp)

# Also (mostly) copied and pasted and shifted around a bit from LLVM Docs - Building LLVM with CMake - Embedding LLVM in your project
# https://releases.llvm.org/3.6.0/docs/CMake.html#embedding-llvm-in-your-project
target_include_directories(coxianc PRIVATE "../include" ${LLVM_INCLUDE_DIRS})

llvm_map_components_to_libnames(llvm_libs core)

message(STATUS ${llvm_libs})
target_link_libraries(coxianc ${llvm_libs})
# end copy and paste

set_target_properties(coxianc PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/bin"
    DEBUG_POSTFIX "d")
