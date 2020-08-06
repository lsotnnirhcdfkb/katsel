add_executable(coxianc main.cpp lexer.cpp parser.cpp errors.cpp astnode.cpp)
target_include_directories(coxianc PRIVATE "../include")

set_target_properties(coxianc PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/bin"
    DEBUG_POSTFIX "d")
