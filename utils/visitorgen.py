#!/usr/bin/env python3
import argparse, sys

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

# generate header file function {{{1
def genHeader():
    # generating header stuff {{{2
    # header stuff
    print('#pragma once')
    print()

    # includes
    print('#include <string>')
    print()

    # forward declarations {{{2
    for astClass in astClasses:
        print(f'class {astClass}AST;')
    print()

    # generating classes {{{2
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

# generate blankvisitor function {{{1
def genBlankVisitor():
    for astClass in astClasses:
        if astClass == '':
            continue
        print(f'void BlankVisitor::visit{astClass}AST(const {astClass}AST *ast) {{}}')


# parse args and run {{{1
parser = argparse.ArgumentParser(description='Generate the visitor header file or the BlankVisitor method implementations.')
parser.add_argument('-e', '--header', action='store_true', help='Generate header file of all the Visitor classes')
parser.add_argument('-b', '--blank', action='store_true', help='Generate BlankVisitor methods')

if len(sys.argv) == 1:
    parser.print_help(sys.stderr)
    sys.exit(1)

args = parser.parse_args()

if args.header:
    genHeader()

if args.blank:
    genBlankVisitor()

