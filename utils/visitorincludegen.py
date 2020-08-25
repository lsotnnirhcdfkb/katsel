#!/usr/bin/env python3

# yes i probably should share this list between astgen.py and this file but i'm too lazy to do that
astclasses = [
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
# the second item are the private fields/methods
visitorClasses = [
    ('', []),
    ('Print', ['int indent', 'bool pindent', 'void print(std::string &str)', 'void print(std::string &&str)']),
    ('Blank', []),
]

# generate visitor declarations
for visitorClass in visitorClasses:
    name = visitorClass[0]
    privatestuff = visitorClass[1]
