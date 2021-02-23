#!/usr/bin/env python3

import helpers

# Classes {{{1
# AST {{{2
class AST:
    def __init__(self, name, base, fields):
        self.name = name
        self.fields = helpers.Field.process(fields)
        self.base = base
# ASTBase {{{2
class ASTBase:
    def __init__(self, name):
        self.name = name
# ASTSuperBase {{{2
class ASTSuperBase:
    def __init__(self):
        self.name ='AST'
# Classes to generate {{{1
asts = [
    ASTSuperBase(),
    ASTBase('CUB'),
    ASTBase('Decl'),
    ASTBase('ImplMember'),
    ASTBase('Stmt'),
    ASTBase('Expr'),
    ASTBase('Type'),
    ASTBase('ParamB'),
    ASTBase('VStmtIB'),
    ASTBase('PathB'),

    # a class to keep track of locations of syntactic elements that don't matter (like
    # line endings where the location matters (so that other statements can use the line
    # ending as an ending position) but does not reduce down to an actual AST,
    # so its location cannot be kept track of otherwise)
    # it should never appear in the final AST, because no fields should have a PureLocationB
    # or a PureLocation as a field
    ASTBase('PureLocationB'),
    AST('PureLocation', 'PureLocationB', 'int|dummy'),

    AST('ImplicitDecl'      , 'Decl', 'int|dummy'),

    AST('CU'                , 'CUB', 'std::vector<std::unique_ptr<Decl>>|decls'),
    AST('ImplDecl'          , 'Decl', 'std::unique_ptr<Type>|impl_for!std::vector<std::unique_ptr<ImplMember>>|members'),
    AST('FunctionDecl'      , 'Decl', 'std::unique_ptr<Type>|retty ! Located<Tokens::Identifier>|name ! std::vector<std::unique_ptr<ParamB>>|params ! std::unique_ptr<Block>|body'),

    AST('FunctionImplMember', 'ImplMember', 'std::unique_ptr<FunctionDecl>|fun'),

    AST('VarStmt'           , 'Stmt', 'std::unique_ptr<Type>|type ! bool|mut ! Located<Tokens::Identifier>|name ! Maybe<Located<Tokens::Equal>>|equal ! std::unique_ptr<Expr>|expr'),

    AST('ExprStmt'          , 'Stmt', 'std::unique_ptr<Expr>|expr'),
    AST('RetStmt'           , 'Stmt', 'std::unique_ptr<Expr>|expr'),

    AST('PathType'          , 'Type', 'std::unique_ptr<Path>|path'),
    AST('PointerType'       , 'Type', 'bool|mut ! std::unique_ptr<Type>|type'),
    AST('ThisType'          , 'Type', 'Located<Tokens::This>|th'),

    AST('Param'             , 'ParamB', 'std::unique_ptr<Type>|type ! Located<Tokens::Identifier>|name ! bool|mut'),
    AST('ThisParam'         , 'ParamB', 'bool|ptr ! bool|mut'),

    AST('Block'             , 'Expr', 'std::vector<std::unique_ptr<Stmt>>|stmts'),
    AST('IfExpr'            , 'Expr', 'Located<Tokens::If>|iftok ! Maybe<Located<Tokens::Else>>|elsetok ! std::unique_ptr<Expr>|cond ! std::unique_ptr<Expr>|trues ! std::unique_ptr<Expr>|falses'),
    AST('WhileExpr'         , 'Expr', 'std::unique_ptr<Expr>|cond ! std::unique_ptr<Expr>|body'),

    AST('AssignmentExpr'    , 'Expr', 'std::unique_ptr<Expr>|target ! Located<AssignOperator>|equal ! std::unique_ptr<Expr>|expr'),
    AST('ShortCircuitExpr'  , 'Expr', 'std::unique_ptr<Expr>|lhs ! Located<ShortCircuitOperator>|op ! std::unique_ptr<Expr>|rhs'),
    AST('BinaryExpr'        , 'Expr', 'std::unique_ptr<Expr>|lhs ! Located<BinaryOperator>|op ! std::unique_ptr<Expr>|rhs'),
    AST('CastExpr'          , 'Expr', 'std::unique_ptr<Type>|type ! std::unique_ptr<Expr>|expr'),
    AST('UnaryExpr'         , 'Expr', 'Located<UnaryOperator>|op ! std::unique_ptr<Expr>|expr'),
    AST('AddrofExpr'        , 'Expr', 'Located<Tokens::Amper>|op ! std::unique_ptr<Expr>|expr ! bool|mut'),
    AST('DerefExpr'         , 'Expr', 'Located<Tokens::Star>|op ! std::unique_ptr<Expr>|expr'),
    AST('CallExpr'          , 'Expr', 'std::unique_ptr<Expr>|callee ! Located<Tokens::OParen>|oparn ! std::vector<std::unique_ptr<Expr>>|args'),
    AST('FieldAccessExpr'   , 'Expr', 'std::unique_ptr<Expr>|operand ! Located<Tokens::Period>|dot ! Located<Tokens::Identifier>|field'),
    AST('MethodCallExpr'    , 'Expr', 'std::unique_ptr<Expr>|operand ! Located<Tokens::Period>|dot ! Located<Tokens::Identifier>|method ! Located<Tokens::OParen>|oparn ! std::vector<std::unique_ptr<Expr>>|args'),
    AST('BoolLit'           , 'Expr', 'Located<Tokens::BoolLit>|val'),
    AST('FloatLit'          , 'Expr', 'Located<Tokens::FloatLit>|val'),
    AST('IntLit'            , 'Expr', 'Located<Tokens::IntLit>|val'),
    AST('CharLit'           , 'Expr', 'Located<Tokens::CharLit>|val'),
    AST('StringLit'         , 'Expr', 'Located<Tokens::StringLit>|val'),
    AST('ThisExpr'          , 'Expr', 'Located<Tokens::This>|tok'),
    AST('PathExpr'          , 'Expr', 'std::unique_ptr<Path>|path'),

    AST('Path'              , 'PathB', 'std::vector<Located<Tokens::Identifier>>|segments'),
]
# Generating methods {{{1
# Generating AST stuff {{{2
# Generate AST declarations {{{3
def gen_ast_decls():
    output = []
    for ast in asts:
        if isinstance(ast, AST):
            output.append(f'class {ast.name} : public {ast.base} {{\n')

            output.append( 'public:\n')
            output.append( '    Maybe<Span const> _span;\n')

            output.append(helpers.Field.as_fields(ast.fields, indent=4))

            output.append(f'    virtual void ast_accept({ast.base}Visitor &v) override;\n')
            output.append( '    virtual Maybe<Span const> const &span() const override;\n')
            output.append(f'    {ast.name}(Maybe<Span const> const &span, {helpers.Field.as_params(ast.fields)});\n')

            output.append( '};\n')
        elif isinstance(ast, ASTBase):
            output.append(f'class {ast.name} : public AST {{\n')
            output.append( 'public:\n')


            output.append(f'    virtual ~{ast.name}() {{}}\n')
            output.append(f'    virtual void ast_accept({ast.name}Visitor &v) = 0;\n')
            output.append( '};\n')
        else:
            output.append(('class AST {\n'
                           'public:\n'
                           '    virtual ~AST() {}\n'
                           '    virtual Maybe<Span const> const &span() const = 0;\n'
                           '};\n'))

    return''.join(output)
# Generate AST definitions {{{3
def gen_ast_defs():
    output = []
    for ast in asts:
        if isinstance(ast, AST):
            output.append(f'ASTNS::{ast.name}::{ast.name}(Maybe<Span const> const &span, {helpers.Field.as_params(ast.fields)}): ')

            output.append(f'_span(span), ')
            output.append(', '.join(f'{field.name}(std::move({field.name}))' for field in ast.fields))

            output.append(' {}\n')

            output.append(f'void ASTNS::{ast.name}::ast_accept(ASTNS::{ast.base}Visitor &v) {{ v.ast_visit(*this); }}\n')
            output.append(f'Maybe<Span const> const &ASTNS::{ast.name}::span() const {{ return _span; }}\n')

    return ''.join(output)
# Generate AST forward decls {{{3
def gen_ast_fwd():
    output = []
    for ast in asts: output.append(f'class {ast.name};\n')
    return ''.join(output)
# Generating Visitor stuff {{{2
# Generate Visitor decls {{{3
def gen_visitor_decls():
    output = []
    for ast in asts:
        if isinstance(ast, ASTBase):
            output.append(f'class {ast.name}Visitor {{\n')
            output.append( 'public:\n')
            output.append(f'    virtual ~{ast.name}Visitor() {{}}\n')
            for _ast in asts:
                if isinstance(_ast, AST) and _ast.base == ast.name:
                    output.append(f'    virtual void ast_visit(ASTNS::{_ast.name} &ast) = 0;\n')
            output.append('};\n')
    return ''.join(output)
# Generate overrided functions for visitor classes {{{3
def gen_visitor_methods(*bases):
    output = []
    for ast in asts:
        if not isinstance(ast, AST):
            continue

        if (ast.base in bases or bases == ('all',)):
            output.append(f'void ast_visit(ASTNS::{ast.name} &ast) override;\n')

    return''.join(output)
# Generate inheriting classes {{{3
def gen_visitor_inherit_all():
    return ',\n'.join([f'public ASTNS::{cl.name}Visitor' for cl in asts if isinstance(cl, ASTBase)]) + '\n'
# Generating printing stuff {{{2
# Genearte print visitor {{{3
def gen_print_visitor_methods():
    output = []
    for ast in asts:
        if not isinstance(ast, AST):
            continue

        output.append(          f'void ASTNS::PrintVisitor::ast_visit(ASTNS::{ast.name} &a) {{\n')
        output.append(          f'    pai("{ast.name} {{\\n");\n')
        output.append(           '    ++indent;\n')
        for field in ast.fields:
            output.append(      f'    pai("{field.type} {field.name} = ");\n')
            output.append(      f'    print_field(*this, a.{field.name});\n')
        output.append(           '    --indent;\n')
        output.append(           '    pai("}\\n");\n')
        output.append(           '}\n')

    return''.join(output)
