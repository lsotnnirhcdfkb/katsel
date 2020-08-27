# COxian
The C++ LLVM-Based Compiler Implementation of the Oxian Language

This is very heavily based off the extremely helpful book "Crafting Interpreters" by Bob Nystrom. Except for the LLVM part, of course.
And not only that, but this is also very heavily based off of the [Kaleidescope Langauge front-end tutorial in the LLVM Documentation Tutorial page thing](https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/index.html).

# Features

- Possibly dysfunctional lexing
- Possibly dysfunctional statement parsing with recursvie descent no precedence parsing so *verbosity galore*
- Possibly dysfunctional code generation to LLVM IR
- Compiling to object code
- Optimizing code with LLVM
- Functions
- expressions
- variables

# Todo

- More types (only uint64 for now because of starting simple becuase LLVM is a very complicated library)

- Control flow
- Classes and polymorphism and all that fun OOP stuff
- Very safe pointers
- Auto memory management with very safe pointers and strict ownership scope rules and stuff
- Lots of syntactic sugar
- Switch statements
- Namespaces
- Importing and stuff
- Threads
- Thread-safety
- Asynchronous functions stuff and coroutines and stuff
- Cool stuff (?)
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
- `main.cpp:readFile` reads the source file
- `main.cpp:compileFile` creates a `Lexer` and a `Parser` instance
- `main.cpp:compileFile` calls `parser->parse()` which returns an `AST`
- `main.cpp:compileFile()` prints the `AST` returned by `parser->parse()`
