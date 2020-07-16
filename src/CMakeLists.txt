add_executable(ctrbelc main.cpp lexer.cpp)
target_include_directories(ctrbelc PRIVATE "../include")
set_target_properties(ctrbelc PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/bin"
    DEBUG_POSTFIX "d")
