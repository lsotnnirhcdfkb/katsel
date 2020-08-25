#!/usr/bin/env python3
import argparse, sys

# class: AstClass {{{1
class AstClass:
    def __init__(self, name, fields=[]):
        self.name = name + 'AST'
        self.fields = fields

    # AstClass.printHFile {{{2
    def printHFile(self):
        output = []

        output.append(    f'class {self.name} : public AST\n')
        output.append(     '{\n')
        output.append(     'public:\n')
        output.append(    f'    {self.name}({self.genConstructArgs()});\n')
        output.append(     '    void accept(Visitor *v) override;\n')
        output.append('\n')
        for field in self.fields:
            output.append(f'    {field.printHFile(False)};\n')
        output.append(     '};\n')

        return ''.join(output)

    # AstClass.printCppFile {{{2
    def printCppFile(self):
        output = []
        output.append(f'{self.name}::{self.name}({self.genConstructArgs()})')

        initializerList = []
        constructorBody = []

        for field in self.fields:
            if field.type_ == AstField.TTOKEN:
                initializerList.append(f'{field.name}({field.name})')
            elif field.type_ == AstField.TUPTR:
                initializerList.append(f'{field.name}(std::move({field.name}))')
            elif field.type_ == AstField.TVECTOR:
                constructorBody.append((
                            f'    this->{field.name}.reserve({field.name}.size());\n'
                            f'    for (std::unique_ptr<AST> &ast : {field.name})\n'
                             '    {\n'
                            f'        this->{field.name}.push_back(std::move(ast));\n'
                             '    }\n'
                            ))

        if len(initializerList):
            output.append(f': {", ".join(initializerList)}')

        if len(constructorBody):
            output.append('\n{\n' + "\n".join(constructorBody) + '}\n')
        else:
            output.append(' {}\n')

        output.append(f'void {self.name}::accept(Visitor *v) {{ v->visit{self.name}(this); }}\n')
        return ''.join(output)

    # AstClass helper methods {{{2
    def genConstructArgs(self):
        return ", ".join(f.printHFile(True) for f in self.fields)

# class: AstBaseClass {{{1
class AstBaseClass:
    def __init__(self):
        self.name = 'AST'

    def printHFile(self):
        return (
        "class AST\n"
        "{\n"
        "public:\n"
        "    virtual ~AST() {}\n"
        "\n"
        "    virtual void accept(Visitor *v) = 0;\n"
        "};\n"
        )
    
    def printCppFile(self):
        return ''

# class: AstField {{{1
class AstField:
    TTOKEN = 0
    TVECTOR = 1
    TUPTR = 2

    # T(ype) to C(ode) Type
    TTOCTYPE = {
        TTOKEN: 'Token',
        TVECTOR: 'std::vector<std::unique_ptr<AST>>',
        TUPTR: 'std::unique_ptr<AST>',
    }

    def __init__(self, name, type_):
        self.name = name
        self.type_ = type_

    def printHFile(self, isArgPrint):
        if isArgPrint and self.type_ == AstField.TVECTOR:
            return f'{AstField.TTOCTYPE[self.type_]} &{self.name}'
        else:
            return f'{AstField.TTOCTYPE[self.type_]} {self.name}'

# generation functions: ast.h ast.cpp files {{{1
# print ast.h file {{{2
def printAstHFile():
    for class_ in astClasses:
        print(class_.printHFile())

# print ast.cpp file {{{2
def printAstCppFile():
    print('#include "ast.h"')
    for class_ in astClasses:
        print(class_.printCppFile())

# generation functions: visitor files {{{1
# generate ast forward declarations for visitor.h {{{2
def printForwardDecl():
    for astClass in astClasses:
        print(f'class {astClass.name};')
# generate visitAST methods {{{2
def printVisitASTMethods(isBase):
    for astClass in astClasses:
        if astClass.name == 'AST':
            continue

        if isBase:
            print('    virtual ', end='')
        else:
            print('    ', end='')

        print(f'void visit{astClass.name}(const {astClass.name} *ast)', end='')

        if isBase:
            print(' = 0;')
        else:
            print(' override;')
# generate BlankVisitor method definitions {{{2
def printBlankVisitorDefinitions():
    for astClass in astClasses:
        if astClass.name == 'AST':
            continue
        print(f'void BlankVisitor::visit{astClass.name}(const {astClass.name} *ast) {{}}')
# lists: ast classes to generate {{{1
astClasses = [
    AstBaseClass(),
    AstClass('Binary'       , [AstField('op', AstField.TTOKEN), AstField('last', AstField.TUPTR), AstField('rast', AstField.TUPTR)]),
    AstClass('TernaryOp'    , [AstField('conditional', AstField.TUPTR), AstField('trueast', AstField.TUPTR), AstField('falseast', AstField.TUPTR)]),
    AstClass('Unary'        , [AstField('op', AstField.TTOKEN), AstField('ast', AstField.TUPTR)]),
    AstClass('Primary'      , [AstField('value', AstField.TTOKEN)]),
    AstClass('ExprStmt'     , [AstField('ast', AstField.TUPTR)]),
    AstClass('Program'      , [AstField('asts', AstField.TVECTOR)]),
    AstClass('Function'     , [AstField('type', AstField.TUPTR), AstField('name', AstField.TTOKEN), AstField('params', AstField.TUPTR), AstField('body', AstField.TUPTR)]),
    AstClass('Block'        , [AstField('stmts', AstField.TVECTOR)]),
    AstClass('Type'         , [AstField('type', AstField.TTOKEN)]),
    AstClass('Param'        , [AstField('type', AstField.TUPTR), AstField('paramname', AstField.TTOKEN)]),
    AstClass('Params'       , [AstField('params', AstField.TVECTOR)]),
    AstClass('VarStmt'      , [AstField('type', AstField.TUPTR), AstField('name', AstField.TTOKEN), AstField('expression', AstField.TUPTR)]),
    AstClass('Assign'       , [AstField('lhs', AstField.TUPTR), AstField('rhs', AstField.TUPTR), AstField('equalSign', AstField.TTOKEN)]),
    AstClass('VariableRef'  , [AstField('var', AstField.TTOKEN)]),
    AstClass('ReturnStmt'   , [AstField('expr', AstField.TUPTR)]),
    AstClass('Arg'          , [AstField('expr', AstField.TUPTR)]),
    AstClass('Args'         , [AstField('args', AstField.TVECTOR)]),
    AstClass('Call'         , [AstField('varrefast', AstField.TUPTR), AstField('arglistast', AstField.TUPTR)]),
]

# entry: actually running the things by parsing args and stuff {{{1
parser = argparse.ArgumentParser(description='Generate AST classes.')
parser.add_argument('--astheader', action='store_true', help='Generate ast.h classes')
parser.add_argument('--astsource', action='store_true', help='Generate ast.cpp')
parser.add_argument('--forwarddecl', action='store_true', help='Generate forward declarations for visitor.h')
parser.add_argument('--visitormethods', action='store_true', help='Generate visitSomethingAST methods')
parser.add_argument('--visitorbasemethods', action='store_true', help='Generate visitSomethingAST methods in the pure virtual Visitor class')
parser.add_argument('--blankvisitor', action='store_true', help='Generate BlankVisitor::visitSomethingAST method definitions')

if len(sys.argv) == 1:
    parser.print_help(sys.stderr)
    sys.exit(1)

args = parser.parse_args()

if args.astheader:
    printAstHFile()

if args.astsource:
    printAstCppFile()

if args.forwarddecl:
    printForwardDecl()

if args.visitormethods:
    printVisitASTMethods(False)

if args.visitorbasemethods:
    printVisitASTMethods(True)

if args.blankvisitor:
    printBlankVisitorDefinitions()
