#!/usr/bin/env python3
## @file astgen.py
#  Generate AST classes
#  Generate AST class related stuff

# Classes {{{1
# ASTClass {{{2
class ASTClass:
    def __init__(self, name, fields=[], extends=None):
        self.name = name
        self.fields = fields
        self.extends = extends
# PureASTClass {{{2
class PureASTClass:
    def __init__(self, name, annotations=[]):
        self.name = name
        self.annotations = annotations
# ASTField {{{2
class ASTField:
    # im for initialization method
    IM_INITIALIZE = 0
    IM_MOVE = 1

    # pm for print method
    PM_CHILD = 0
    PM_TOKEN = 1
    PM_ITERATE_CHILD = 2

    def __init__(self, type_, name, passRef, initializationMethod, printMethod):
        self.type_ = type_
        self.name = name
        self.passRef = passRef
        self.initializationMethod = initializationMethod
        self.printMethod = printMethod

    @staticmethod
    def tokenField(name):
        return ASTField('Token', name, False, ASTField.IM_INITIALIZE, ASTField.PM_TOKEN)
    @staticmethod
    def uptrField(pointto, name):
        return ASTField(f'std::unique_ptr<{pointto}>', name, False, ASTField.IM_MOVE, ASTField.PM_CHILD)
    @staticmethod
    def opField():
        return ASTField.tokenField('op')
    @staticmethod
    def exprField(name):
        return ASTField.uptrField('Expr', name)
# Classes to generate {{{1
asts = [
    PureASTClass('Expr'),
    PureASTClass('Decl'),
    PureASTClass('Type'),
    PureASTClass('Stmt'),

    ASTClass('Program', fields=[
            ASTField('std::vector<std::unique_ptr<Decl>>', 'decls', True, ASTField.IM_MOVE, ASTField.PM_ITERATE_CHILD),
        ]),

    ASTClass('BinaryExpr', fields=[
            ASTField.exprField('lhs'),
            ASTField.exprField('rhs'),
            ASTField.opField(),
        ], extends='Expr'),

    ASTClass('TernaryExpr', fields=[
            ASTField.exprField('condition'),
            ASTField.exprField('trues'),
            ASTField.exprField('falses'),
        ], extends='Expr'),

    ASTClass('UnaryExpr', fields=[
            ASTField.exprField('operand'),
            ASTField.opField(),
        ], extends='Expr'),

    ASTClass('PrimaryExpr', fields=[
            ASTField.tokenField('value'),
        ], extends='Expr'),

    ASTClass('CallExpr', fields=[
            ASTField.exprField('func'),
            ASTField.uptrField('Arg', 'args'),
        ], extends='Expr'),

    ASTClass('BlockStmt', fields=[
            ASTField('std::vector<std::unique_ptr<Stmt>>', 'stmts', True, ASTField.IM_MOVE, ASTField.PM_ITERATE_CHILD),
        ], extends='Stmt'),

    ASTClass('ExprStmt', fields=[
            ASTField.exprField('expr'),
        ], extends='Stmt'),

    ASTClass('ReturnStmt', fields=[
            ASTField.exprField('val'),
        ], extends='Stmt'),

    ASTClass('VarStmt', fields=[
            ASTField.uptrField('Type', 'type'),
            ASTField.tokenField('name'),
            ASTField.uptrField('Expr', 'assign')
        ], extends='Stmt'),

    ASTClass('BaseType', fields=[
            ASTField.tokenField('type'),
        ], extends='Type'),

    ASTClass('FunctionDecl', fields=[
            ASTField.uptrField('Type', 'rettype'),
            ASTField.tokenField('name'),
            ASTField.uptrField('Param', 'params'),
            ASTField.uptrField('BlockStmt', 'block'),
        ], extends='Decl'),

    ASTClass('GlobalVarDecl', fields=[
            ASTField.uptrField('Type', 'type'),
            ASTField.tokenField('name'),
            ASTField.exprField('value'),
        ], extends='Decl'),

    ASTClass('Param', fields=[
            ASTField.uptrField('Type', 'type'),
            ASTField.tokenField('name'),
            ASTField.uptrField('Param', 'next'),
        ]),

    ASTClass('Arg', fields=[
            ASTField.exprField('value'),
            ASTField.uptrField('Arg', 'next'),
        ])
]
# Generating methods {{{1
# Generating helper methods {{{2
def asArgument(field):
    return f'{field.type_} {"&" if field.passRef else ""}{field.name}'
def asDeclaration(field):
    return f'{field.type_} {field.name};'
def fieldInitialziation(field):
    if field.initializationMethod == ASTField.IM_INITIALIZE:
        return f'{field.name}({field.name})'
    elif field.initializationMethod == ASTField.IM_MOVE:
        return f'{field.name}(std::move({field.name}))'
    else:
        raise Exception(f'Inavlid initialization method {field.initializationMethod}')
# Generating AST stuff {{{2
# Generate AST declarations {{{3
def genASTDecls():
    output = []
    for ast in asts:
        output.append(f'    class {ast.name};\n')

    for ast in asts:
        if type(ast) != PureASTClass:
            if ast.extends is not None:
                output.append(f'    class {ast.name} : public {ast.extends}\n')
            else:
                output.append(f'    class {ast.name}\n')

            output.append( '    {\n')
            output.append( '    public:\n')
            output.append(f'        {ast.name}({", ".join(asArgument(field) for field in ast.fields)});\n')

            for field in ast.fields:
                output.append('        ')
                output.append(asDeclaration(field))
                output.append('\n')

            output.append(f'        virtual void accept({ast.name if ast.extends is None else ast.extends}Visitor *v);\n')

            if ast.extends is None:
                output.append(f'        virtual ~{ast.name}() {{}}\n')

            output.append( '    };\n')
        else:
            output.append(f'    class {ast.name}\n')
            output.append( '    {\n')
            output.append( '    public:\n')
            output.append(f'        virtual ~{ast.name}() {{}}\n')

            for annotation in ast.annotations:
                annotationvName = annotation[0].lower() + annotation[1:]
                output.append(f'        {annotation} {annotationvName};\n')

            output.append(f'        virtual void accept({ast.name}Visitor *v) = 0;\n')

            output.append( '    };\n')

    return ''.join(output)
# Generate AST definitions {{{3
def genASTDefs():
    output = ['#include "parse/ast.h"\n']
    for ast in asts:
        if type(ast) != PureASTClass:
            output.append(f'ASTNS::{ast.name}::{ast.name}({", ".join(asArgument(field) for field in ast.fields)}): ')

            initializerList = []
            for field in ast.fields:
                initializerList.append(fieldInitialziation(field))

            output.append(', '.join(initializerList))
            output.append(' {}\n')

            output.append(f'void ASTNS::{ast.name}::accept({ast.name if ast.extends is None else ast.extends}Visitor *v) {{ v->visit{ast.name}(this); }}\n')

    return ''.join(output)
# Generating Visitor stuff {{{2
# Generate AST forward declarations {{{3
def genASTForwDecls():
    output = []
    for ast in asts:
        output.append(f'class {ast.name};\n')
    return ''.join(output)
# Generate pure Visitor declarations {{{3
def genPureASTVisitorDecls():
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
# Generate pure Visitor destructors {{{3
def genPureASTVisitorDestructs():
    genclasses = [x for x in asts if type(x) == PureASTClass or x.extends is None]

    output = []
    for genclass in genclasses:
        output.append(f'{genclass.name}Visitor::~{genclass.name}Visitor() {{}}\n')

    return ''.join(output)
# Generate printing stuff {{{2
def genPrintVisitorMethods():
    output = []
    for ast in asts:
        if type(ast) == PureASTClass:
            continue

        output.append(        f'void PrintVisitor::visit{ast.name}(ASTNS::{ast.name} *a)\n')
        output.append(         '{\n')
        output.append(        f'    pai("{ast.name}\\n");\n')
        output.append(        f'    ++indent;\n')
        for field in ast.fields:
            output.append(    f'    pai("{field.name} =");\n')
            if field.printMethod == ASTField.PM_CHILD:
                output.append(f'    if (a->{field.name})\n')
                output.append( '    {\n')
                output.append( '        ++indent;\n')
                output.append( '        pai("\\n");\n')
                output.append(f'        a->{field.name}->accept(this);\n')
                output.append( '        --indent;\n')
                output.append( '    }\n')
                output.append( '    else\n')
                output.append( '    {\n')
                output.append( '        pai(" nullptr\\n");\n')
                output.append( '    }\n')
            elif field.printMethod == ASTField.PM_TOKEN:
                output.append( '    pai(" [");\n')
                output.append(f'    pai(std::string(a->{field.name}.start, a->{field.name}.end));\n')
                output.append( '    pai("]\\n");\n')
            elif field.printMethod == ASTField.PM_ITERATE_CHILD:
                output.append( '    pai("\\n");\n')
                output.append( '    ++indent;\n');
                output.append(f'    for (auto &i : a->{field.name})\n')
                output.append( '    {\n')
                output.append( '        pai("- ");\n')
                output.append(f'        i->accept(this);\n')
                output.append( '    }\n')
                output.append( '    --indent;\n');
            else:
                raise Exception(f'Invalid print method {field.printMethod}')
        output.append(        f'    --indent;\n')
        output.append(         '}\n')

    return ''.join(output)
