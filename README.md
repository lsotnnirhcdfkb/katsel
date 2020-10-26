# COxian
The C++ LLVM-Based Compiler Implementation of the Oxian Language

This is very heavily based off the extremely helpful book "Crafting Interpreters" by Bob Nystrom. Except for the LLVM part, of course.
And not only that, but this is also very heavily based off of the [Kaleidescope Langauge front-end tutorial in the LLVM Documentation Tutorial page thing](https://llvm.org/docs/tutorial/MyFirstLanguageFrontend/index.html).
AND NOt only that, but I also got a lot of helpful information from looking at the source code and documentation of Clang, and seeing how the people developing Clang solve the problems and challenges that I also happen to have.

# Features

- [x] Possibly dysfunctional lexing
- [x] Rewritten parser
- [x] Possibly dysfunctional code generation to LLVM IR
- [ ] Compiling to object code
- [ ] Optimizing code with LLVM
- [x] Functions
- [x] Expressions
- [x] Variables
- [ ] Function overloading
- [ ] Arrays
- [ ] More types
- [ ] Control flow
- [ ] Pointers
- [ ] Auto memory management with very safe pointers and strict ownership scope rules and stuff
- [ ] Global variables
- [ ] Explicit namespaces
- [ ] Classes
- [ ] Templates
- [ ] Const variables (with `var const type thing = value`)
- [ ] Standard libary
- [ ] Function pointers (with `var rettype(paramtypes, paramtypes) variable = &f`)
- [ ] Lots of syntactic sugar
- [ ] Switch statements
- [ ] Importing and stuff
- [ ] Threads
- [ ] Thread-safety
- [ ] Asynchronous functions stuff and coroutines and stuff
- [ ] Cool stuff (?)
- [ ] More

# To do things
You need Python, PyYAML, colorama (for testing script color coding), and LLVM
and I think thats it
