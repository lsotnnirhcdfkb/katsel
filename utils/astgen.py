#!/usr/bin/env python3
## @file astgen.py
#  Generate AST classes
#  Generate AST class related stuff

# ASTClass {{{1
class ASTClass:
    def __init__(self, name, fields=[], extends=[]):
        self.name = name
        self.fields = fields
        self.extends = extends

    def declaration(self):
        output = []
        if len(self.extends):
            output.append(f'    class {self.name} : {", ".join(extend for extend in self.extends)}\n')
        else:
            output.append(f'    class {self.name}\n')

        output.append( '    {\n')
        output.append( '    public:\n')
        output.append(f'        {self.name}({", ".join(field.asArgument() for field in self.fields)});\n')
        if len(self.fields):
            output.append(f'\n')
            for field in self.fields:
                output.append('        ')
                output.append(field.asDeclaration())
                output.append('\n')

        output.append( '    };\n')

        return ''.join(output)

    def forwDecl(self):
        return 'class {self.name} : {", ".join(extend for extend in self.extends)}' if len(self.extends) else 'class {self.name}'

    def definition(self):
        output = []
        output.append(f'ASTNS::{self.name}::{self.name}({", ".join(field.asArgument() for field in self.fields)})\n')
        output.append( '{\n')
        for field in self.fields:
            output.append('    ')
            output.append(field.initialization())
            output.append('\n')
        output.append( '}\n')
        return ''.join(output)

# PureASTClass {{{1
class PureASTClass:
    def __init__(self, name):
        self.name = name

    def declaration(self):
        output = []
        output.append(f'    class {self.name}\n')
        output.append( '    {\n')
        output.append( '    public:\n')
        output.append(f'        virtual ~{self.name}() {{}}\n')
        output.append( '    };\n')

        return ''.join(output)

    def definition(self):
        return ''

# ASTField {{{1
class ASTField:
    # im for initialization method
    IM_ASSIGN = 0
    IM_MOVE = 1
    IM_ITERATE_MOVE = 2

    def __init__(self, type_, name, passRef=False, initializationMethod=None):
        self.type_ = type_
        self.name = name
        self.passRef = passRef
        self.initializationMethod = ASTField.IM_ASSIGN if initializationMethod is None else initializationMethod

    def asArgument(self):
        return f'{self.type_} {"&" if self.passRef else ""}{self.name}'

    def asDeclaration(self):
        return f'{self.type_} {self.name};'

    def initialization(self):
        if self.initializationMethod == ASTField.IM_ASSIGN:
            return f'{self.name} = {self.name};'

        if self.initializationMethod == ASTField.IM_MOVE:
            return f'{self.name} = std::move({self.name});'

        if self.initializationMethod == ASTField.IM_ITERATE_MOVE:
            return f'for (auto &p : {self.name}) {self.name}.push_back(std::move(p));'

# constants {{{1
SRCOUTDIR = 'src/asts'
HOUTDIR = 'include/'
# common types {{{1
def tokenField(name):
    return ASTField('Token', name, False)
def uptrField(pointto, name):
    return ASTField(f'std::unique_ptr<{pointto}>', name, False, ASTField.IM_MOVE)
def opField():
    return ASTField('Token', 'op', False)
def exprField(name):
    return uptrField('Expr', name)
# lists: ast classes to generate {{{1
asts = [
    PureASTClass('Expr'),
    PureASTClass('Decl'),
    PureASTClass('Type'),
    PureASTClass('LValue'),
    PureASTClass('Stmt'),

    ASTClass('Program', fields=[
            ASTField('std::vector<Decl>', 'decls', True, ASTField.IM_ITERATE_MOVE),
        ]),

    ASTClass('BinaryExpr', fields=[
            exprField('lhs'),
            exprField('rhs'),
            opField(),
        ], extends=['Expr']),

    ASTClass('TernaryExpr', fields=[
            exprField('condition'),
            exprField('trues'),
            exprField('falses'),
        ], extends=['Expr']),

    ASTClass('UnaryExpr', fields=[
            exprField('operand'),
            opField(),
        ], extends=['Expr']),

    ASTClass('PrimaryExpr', fields=[
            tokenField('value'),
        ], extends=['Expr']),

    ASTClass('AssignExpr', fields=[
            uptrField('LValue', 'assignee'),
            exprField('value'),
        ], extends=['Expr']),

    ASTClass('CallExpr', fields=[
            uptrField('LValue', 'func'),
            uptrField('Arg', 'args'),
        ], extends=['Expr']),

    ASTClass('BlockStmt', fields=[
            ASTField('std::vector<Stmt>', 'stmts', True, ASTField.IM_ITERATE_MOVE),
        ], extends=['Stmt']),

    ASTClass('ExprStmt', fields=[
            exprField('expr'),
        ], extends=['Stmt']),

    ASTClass('ReturnStmt', fields=[
            exprField('val'),
        ], extends=['Stmt']),

    ASTClass('VarStmt', fields=[
            uptrField('Type', 'type'),
            tokenField('name'),
            exprField('value'),
        ], extends=['Stmt']),

    ASTClass('VarRef', fields=[
            tokenField('var'),
        ], extends=['LValue']),

    ASTClass('BaseType', fields=[
            tokenField('type'),
        ], extends=['Type']),

    ASTClass('FunctionDecl', fields=[
            uptrField('Type', 'type'),
            tokenField('name'),
            uptrField('BlockStmt', 'block'),
        ], extends=['Decl']),

    ASTClass('GlobalVarDecl', fields=[
            uptrField('Type', 'type'),
            tokenField('name'),
            exprField('value'),
        ], extends=['Decl']),

    ASTClass('Param', fields=[
            uptrField('Type', 'type'),
            tokenField('name'),
            uptrField('Param', 'next'),
        ]),

    ASTClass('Arg', fields=[
            exprField('value'),
            uptrField('Arg', 'next'),
        ])
]

# generate h file with declarations {{{1
def genHFile():
    output = []
    for ast in asts:
        output.append(f'    class {ast.name};\n')
    output.append('\n')

    for ast in asts:
        output.append(ast.declaration())

    return ''.join(output)
# generate definitions {{{1
def genCppFile():
    output = ['#include "ast.h"\n']
    for ast in asts:
        output.append(ast.definition())

    return ''.join(output)
