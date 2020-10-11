#!/usr/bin/env python3
## @file astgen.py
#  Generate AST classes
#  Generate AST class related stuff

# ASTClass {{{1
class ASTClass:
    def __init__(self, name, fields=[], extends=None, annotations=[]):
        self.name = name
        self.fields = fields
        self.extends = extends
        self.annotations = annotations

    def declaration(self):
        output = []
        if self.extends is not None:
            output.append(f'    class {self.name} : public {self.extends}\n')
        else:
            output.append(f'    class {self.name}\n')

        output.append( '    {\n')
        output.append( '    public:\n')
        output.append(f'        {self.name}({", ".join(field.asArgument() for field in self.fields)});\n')
        if len(self.fields):
            for field in self.fields:
                output.append('        ')
                output.append(field.asDeclaration())
                output.append('\n')

        for annotation in self.annotations:
            annotationvName = annotation[0].lower() + annotation[1:]
            output.append(f'        {annotation} {annotationvName};\n')

        output.append(f'        virtual void accept({self.name if self.extends is None else self.extends}Visitor *v);\n')

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
        output.append(f'void ASTNS::{self.name}::accept({self.name if self.extends is None else self.extends}Visitor *v) {{ v->visit{self.name}(this); }}\n')
        return ''.join(output)

# PureASTClass {{{1
class PureASTClass:
    def __init__(self, name, annotations=[]):
        self.name = name
        self.annotations = annotations

    def declaration(self):
        output = []
        output.append(f'    class {self.name}\n')
        output.append( '    {\n')
        output.append( '    public:\n')
        output.append(f'        virtual ~{self.name}() {{}}\n')
        for annotation in self.annotations:
            annotationvName = annotation[0].lower() + annotation[1:]
            output.append(f'        {annotation} {annotationvName};\n')
        output.append(f'        virtual void accept({self.name}Visitor *v) = 0;\n')
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
            return f'this->{self.name} = {self.name};'

        if self.initializationMethod == ASTField.IM_MOVE:
            return f'this->{self.name} = std::move({self.name});'

        if self.initializationMethod == ASTField.IM_ITERATE_MOVE:
            return f'for (auto &p : {self.name}) this->{self.name}.push_back(std::move(p)); {self.name}.clear();'

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
annotations = {
    'ExprAn': [('bool', 'isLValue'), ('int', 'type')],
    'FuncDeclAn': [('int', 'retType'), ('int', 'nArgs')],
}
asts = [
    PureASTClass('Expr', annotations=['ExprAn']),
    PureASTClass('Decl'),
    PureASTClass('Type'),
    PureASTClass('Stmt'),

    ASTClass('Program', fields=[
            ASTField('std::vector<std::unique_ptr<Decl>>', 'decls', True, ASTField.IM_ITERATE_MOVE),
        ]),

    ASTClass('BinaryExpr', fields=[
            exprField('lhs'),
            exprField('rhs'),
            opField(),
        ], extends='Expr'),

    ASTClass('TernaryExpr', fields=[
            exprField('condition'),
            exprField('trues'),
            exprField('falses'),
        ], extends='Expr'),

    ASTClass('UnaryExpr', fields=[
            exprField('operand'),
            opField(),
        ], extends='Expr'),

    ASTClass('PrimaryExpr', fields=[
            tokenField('value'),
        ], extends='Expr'),

    ASTClass('AssignExpr', fields=[
            uptrField('Expr', 'assignee'),
            exprField('value'),
        ], extends='Expr'),

    ASTClass('CallExpr', fields=[
            uptrField('Expr', 'func'),
            uptrField('Arg', 'args'),
        ], extends='Expr'),

    ASTClass('LtoRVExpr', fields=[
            uptrField('Expr', 'val')
        ], extends='Expr'),

    ASTClass('BlockStmt', fields=[
            ASTField('std::vector<std::unique_ptr<Stmt>>', 'stmts', True, ASTField.IM_ITERATE_MOVE),
        ], extends='Stmt'),

    ASTClass('ExprStmt', fields=[
            exprField('expr'),
        ], extends='Stmt'),

    ASTClass('ReturnStmt', fields=[
            exprField('val'),
        ], extends='Stmt'),

    ASTClass('VarStmt', fields=[
            uptrField('Type', 'type'),
            tokenField('name'),
            exprField('value'),
        ], extends='Stmt'),

    ASTClass('VarRef', fields=[
            tokenField('var'),
        ], extends='Expr'),

    ASTClass('BaseType', fields=[
            tokenField('type'),
        ], extends='Type'),

    ASTClass('FunctionDecl', fields=[
            uptrField('Type', 'rettype'),
            tokenField('name'),
            uptrField('Param', 'params'),
            uptrField('BlockStmt', 'block'),
        ], extends='Decl', annotations=['FuncDeclAn']),

    ASTClass('GlobalVarDecl', fields=[
            uptrField('Type', 'type'),
            tokenField('name'),
            exprField('value'),
        ], extends='Decl'),

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
# generate annotation structs {{{1
def genAnnotationStructs():
    output = []
    for annotationn, annotationfs in annotations.items():
        output.append(f'''struct {annotationn}
{{
''')
        output.append(f'    bool valid = false;\n')
        for annotationfty, annotationfn in annotationfs:
            output.append(f'    {annotationfty} {annotationfn};\n')
        output.append('''};
''')

    return ''.join(output)
# generate forward declarations for visitor.h {{{1
def genASTForwDecls():
    output = []
    for ast in asts:
        output.append(f'class {ast.name};\n')
    return ''.join(output)
# generate pure ast visitor classes {{{1
def genPureASTVisitClasses():
    genclasses = [x for x in asts if type(x) == PureASTClass or x.extends is None]

    output = []
    for genclass in genclasses:
        output.append(f'''class {genclass.name}Visitor
{{
public:
''')

        for ast in asts:
            if type(ast) != PureASTClass and (ast.extends == genclass.name or ast.name == genclass.name):
                output.append(f'    virtual void visit{ast.name}(ASTNS::{ast.name} *a) = 0;\n')

        output.append(f'''    virtual ~{genclass.name}Visitor();
}};
''')

    return ''.join(output)

# generate pure visitor destructors {{{1
def genPureDestructs():
    genclasses = [x for x in asts if type(x) == PureASTClass or x.extends is None]

    output = []
    for genclass in genclasses:
        output.append(f'{genclass.name}::~{genclass.name}() {{}}\n')

    return ''.join(output)
