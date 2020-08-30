# COxian
The C++ LLVM-Based Compiler Implementation of the Oxian Language

This is very heavily based off the extremely helpful book "Crafting Interpreters" by Bob Nystrom. Except for the LLVM part, of course.
And not only that, but this is also very heavily based off of the [Kaleidescope Langauge front-end tutorial in the LLVM Documentation Tutorial page thing](https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/index.html).

# Features

- [x] Possibly dysfunctional lexing
- [x] Possibly dysfunctional statement parsing with recursvie descent no precedence parsing so *verbosity galore*
- [x] Possibly dysfunctional code generation to LLVM IR
- [x] Compiling to object code
- [x] Optimizing code with LLVM
- [x] Functions
- [x] Expressions
- [x] Variables
- [ ] More types (only uint64 for now because of starting simple becuase LLVM is a very complicated library)
- [ ] Control flow
- [ ] Classes and polymorphism and all that fun OOP stuff
- [ ] Very safe pointers
- [ ] Auto memory management with very safe pointers and strict ownership scope rules and stuff
- [ ] Lots of syntactic sugar
- [ ] Switch statements
- [ ] Namespaces
- [ ] Importing and stuff
- [ ] Threads
- [ ] Thread-safety
- [ ] Asynchronous functions stuff and coroutines and stuff
- [ ] Cool stuff (?)
- [ ] More
