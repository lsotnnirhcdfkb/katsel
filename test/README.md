# `test` directory

This directory incldues test file sources.
Each of these sources compile into a binary in the `bin/test` directory.
Each of these binaries can be run and each will output something along the lines of "test completed" or "test failed" depending on whether the test failed.
Also, there will be a non-zero exit code if the test fails.

The only tests that there are now are:
- `lexertest.cpp`: Testing the lexer and its ability to identify token types correctly
- `CMakeLists.txt`: The file which tells CMake how the build system should compile the tests
- `README.md`: A file explaining the tests that you probably could figure out yourself
