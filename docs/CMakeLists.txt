
if (${GENDOC})
    find_package(Doxygen REQUIRED)

    set(DOXYGEN_OUTPUT_DIR ${CMAKE_BINARY_DIR}/docs)
    set(DOXYFILE_IN ${CMAKE_SOURCE_DIR}/docs/Doxyfile)
    set(DOXYFILE_OUT ${CMAKE_BINARY_DIR}/docs/Doxyfile)

    configure_file(${DOXYFILE_IN} ${DOXYFILE_OUT} @ONLY)

    file(MAKE_DIRECTORY ${DOXYGEN_OUTPUT_DIR})

    add_custom_target(doxygendocs ALL 
            COMMAND ${DOXYGEN_EXECUTABLE} ${DOXYFILE_OUT}
            WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
            COMMENT "Generating docs with Doxygen"
            VERBATIM)
endif()
