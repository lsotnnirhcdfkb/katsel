#!/usr/bin/env python3

# T at the beginning means types
# these are the only types that the asts have for now
TTOKEN = 0
TVECTOR = 1
TUPTR = 2

# T(ype) to C(ode) Type
TTOCTYPE = {
    TTOKEN: 'Token',
    TVECTOR: 'std::vector<std::unique_ptr<AST>>',
    TUPTR: 'std::unique_ptr<AST>',
}

# ac short for ast class
class ac:
    def __init__(self, name, fields=[]):
        self.name = name + 'AST'
        self.fields = fields

    def printHFile(self):
        # print(('class {name}'

        print(f'class {self.name} : public AST')
        print( '{')
        print( 'public:')
        print(f'    {self.name}({", ".join(f.printHFile(True) for f in self.fields)});')
        print( '    void accept(Visitor *v) override;')
        print( )
        for field in self.fields:
            print(f'    {field.printHFile(False)};')
        print( '};')
        print( '')

# abc short for ast base class
class abc:
    def printHFile(self):
        print((
        "class AST\n"
        "{\n"
        "public:\n"
        "    virtual ~AST() {}\n"
        "\n"
        "    virtual void accept(Visitor *v) = 0;\n"
        "};\n"
        ))
        
# af short for ast field
class af:
    def __init__(self, name, type_):
        self.name = name
        self.type_ = type_

    def printHFile(self, isArgPrint):
        if isArgPrint and self.type_ == TVECTOR:
            return f'{TTOCTYPE[self.type_]} &{self.name}'
        else:
            return f'{TTOCTYPE[self.type_]} {self.name}'

def printHFile():
    print('#pragma once\n')
    for include in includes:
        print(f'#include {include}')

    print()

    for class_ in classes:
        class_.printHFile()

includes = [
        '<vector>',
        '<string>',
        '<iostream>',
        '<memory>',
        '"token.h"',
        '"visitor.h"'
    ]

classes = [
    abc(),
    ac('Binary'       , [af('op', TTOKEN), af('last', TUPTR), af('rast', TUPTR)]),
    ac('TernaryOp'    , [af('conditional', TUPTR), af('trueast', TUPTR), af('falseast', TUPTR)]),
    ac('Unary'        , [af('op', TTOKEN), af('ast', TUPTR)]),
    ac('Primary'      , [af('value', TTOKEN)]),
    ac('ExprStmt'     , [af('ast', TUPTR)]),
    ac('Program'      , [af('asts', TVECTOR)]),
    ac('Function'     , [af('type', TUPTR), af('name', TTOKEN), af('params', TUPTR), af('body', TUPTR)]),
    ac('Block'        , [af('stmts', TVECTOR)]),
    ac('Type'         , [af('type', TTOKEN)]),
    ac('Param'        , [af('type', TUPTR), af('paramname', TTOKEN)]),
    ac('Params'       , [af('params', TVECTOR)]),
    ac('VarStmt'      , [af('type', TUPTR), af('name', TTOKEN), af('expression', TUPTR)]),
    ac('Assign'       , [af('lhs', TUPTR), af('rhs', TUPTR), af('equalSign', TTOKEN)]),
    ac('VariableRef'  , [af('var', TTOKEN)]),
    ac('ReturnStmt'   , [af('expr', TUPTR)]),
    ac('Arg'          , [af('expr', TUPTR)]),
    ac('Args'         , [af('args', TVECTOR)]),
    ac('Call'         , [af('varrefast', TUPTR), af('arglistast', TUPTR)]),
]

printHFile()

