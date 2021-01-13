if (${DOTEST})
    set(TESTPY ${CMAKE_SOURCE_DIR}/test/testall.py)
    add_custom_command(OUTPUT ${CMAKE_BINARY_DIR}/test.timestamp
        COMMAND ${Python_EXECUTABLE} ${TESTPY} ${CMAKE_BINARY_DIR}/bin/katselc ${CMAKE_CXX_COMPILER}
        COMMAND cmake -E touch ${CMAKE_BINARY_DIR}/test.timestamp
        DEPENDS katselc
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/test
        COMMENT "Testing"
        VERBATIM)
    add_custom_target(test ALL
        DEPENDS ${CMAKE_BINARY_DIR}/test.timestamp)
endif()
