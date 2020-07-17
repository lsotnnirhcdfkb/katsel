add_executable(coxianc main.cpp)
set_target_properties(coxianc PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/bin"
    DEBUG_POSTFIX "d")
