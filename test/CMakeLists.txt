
# lexing test (test/lexertest.cpp)
add_executable(lexertest lexertest.cpp ../src/lexer.cpp)
target_include_directories(lexertest PRIVATE ../include)

set_target_properties(lexertest PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/bin/test"
    DEBUG_POSTFIX "d")

