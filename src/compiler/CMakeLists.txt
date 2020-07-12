add_executable(ctrbelc main.cpp)
set_target_properties(ctrbelc PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/bin"
    DEBUG_POSTFIX "d")
