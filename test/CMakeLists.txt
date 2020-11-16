set(TESTPY ${CMAKE_SOURCE_DIR}/test/testall.py)
add_custom_target(test ALL
    COMMAND ${Python_EXECUTABLE} ${TESTPY} ${CMAKE_BINARY_DIR}/bin/katselc
    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/test
    COMMENT "Testing"
    VERBATIM)
