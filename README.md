# COxian
The C++ LLVM-Based Compiler Implementation of the Oxian Language

This is very heavily based off the extremely helpful book "Crafting Interpreters" by Bob Nystrom. Except for the LLVM part, of course.

# Features

- Possibly dysfunctional lexing
- Possibly dysfunctional parsing of an expression only grammar

# Todo

- Parsing more general syntax
- Compilation
- Optimization
- LLVM fun stuff

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

- `include`: header files
- `reference`: reference files in plaintext for looking at a list of tokens and grammar, etc
- `src`: source files
- `utils`: utility code generation files

Here is more terrible documentation about what each file means:
- `CMakeLists.txt`: CMakeLists file
- `LICENSE`: License (I don't know how to use this also it's probably outdated like at the time of writing the line it isn't because I updated the year the other day but it probably soon will be outdated)
- `README.md`: This file

Each folder *should* have more documentation about what each file means, but I am really bad at doing documentation so who knows what will be there.

## Compiler overview
The compiling pipeline is as follows:

- `main.cpp` is invoked with a filename.
- `int main()` calls `void compileFile()`
- `void compileFile()` allocates a new `Lexer` object on the heap with a unique pointer
- `void compileFile()` allocates a new `Parser` object on the heap with a unique pointer
- `void compileFile()` calls `parser->expression()` in order to parse the grammar
- `void compileFile()` prints the `ASTNode` returned by `parser->expression()`
