#!/usr/bin/env python3

# lists and stuff {{{1
# yes i probably should share this list between astgen.py and this file but i'm too lazy to do that
astClasses = [
    '',
    'Binary',
    'TernaryOp',
    'Unary',
    'Primary',
    'ExprStmt',
    'Program',
    'Function',
    'Block',
    'Type',
    'Param',
    'Params',
    'VarStmt',
    'Assign',
    'VariableRef',
    'ReturnStmt',
    'Arg',
    'Args',
    'Call',
]

# first item is the name of the class
# if it's blank then that's the base class
# the second item is a list of the public fields/methods 
# the third item is a list of the private fields/methods
visitorClasses = [
    ('', [], []),
    ('Print', ['PrintVisitor()'], ['int indent', 'bool pindent', 'void print(std::string &str)', 'void print(std::string &&str)']),
    ('Blank', [], []),
]

# generate visitor declarations

# generating header stuff {{{1
# header stuff
print('#pragma once')
print()

# includes
print('#include <string>')
print()

# forward declarations {{{1
for astClass in astClasses:
    print(f'class {astClass}AST;')
print()

# generating classes {{{1
for visitorClass in visitorClasses:
    name = visitorClass[0]
    publicThings = visitorClass[1]
    privateThings = visitorClass[2]

    if name == '':
        print('class Visitor')
    else:
        print(f'class {name}Visitor : public Visitor')

    print('{')
    print('public:')
    
    for publicThing in publicThings:
        print('    ' + publicThing + ';')

    for astClass in astClasses:
        if astClass == '':
            continue

        if name == '':
            print('    virtual ', end='')
        else:
            print('    ', end='')

        print(f'void visit{astClass}AST(const {astClass}AST *ast)', end='')

        if name == '':
            print(' = 0;')
        else:
            print(' override;')

    if len(privateThings):
        print('\nprivate:')
        for privateThing in privateThings:
            print('    ' + privateThing + ';')

    print('};')
    print()

