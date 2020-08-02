# COxian
The C++ LLVM-Based Compiler Implementation of the Oxian Language

This is heavily based off the extremely helpful book "Crafting Interpreters" by Bob Nystrom. Except for the LLVM part, of course.

# Features

- Possibly dysfunctional lexing

# Todo

- Parsing
- Compilation
- Optimization

- Control flow
- Functions
- Classes
- Very safe pointers
- Auto memory management with very safe pointers and strict ownership scope rules and stuff
- Lots of syntactic sugar
- Case statements
- Namespaces
- Importing and stuff
- More

# Help

Here is my really terrible documentation for what the directory stucture is:

- `bin`: binaries
- `include`: header files
- `reference`: reference files in plaintext for looking at a list of tokens and grammar, etc
- `src`: source files
- `test`: test files
- `utils`: utility code generation files

Here is more terrible documentation about what each file means:
- `CMakeLists.txt`: CMakeLists file
- `LICENSE`: License (I don't know how to use this also it's probably outdated like at the time of writing the line it isn't because I updated the year the other day but it probably soon will be outdated)
- `README.md`: This file

Each folder *should* have more documentation about what each file means, but I am really bad at doing documentation so who knows what will be there.

## Pipeline
The compiling pipeline is as follows:

- `main.cpp` is invoked with a filename.
- `main.cpp` creates a Lexer instance
- `main.cpp` creates a Parser instance that has a reference to the lexer instance
- `main.cpp` uses the parser to parse the extremely limited grammar of Oxian that consists of 3 rules (the parser calls the lexer's `nextToken()` method as needed)
- `main.cpp` literally is not even linked with LLVM yet, that's how far this language is

