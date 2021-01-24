#!/usr/bin/env python3

# Classes {{{1
# AST {{{2
class AST:
    def __init__(self, name, base, fields):
        self.name = name
        def process_field(field):
            return (item.strip() for item in field.split('|'))
        self.fields = [ASTField(*process_field(field)) for field in fields.split(',')]
        self.base = base
# ASTBase {{{2
class ASTBase:
    def __init__(self, name):
        self.name = name
# ASTSuperBase {{{2
class ASTSuperBase:
    def __init__(self):
        self.name ='AST'
# ASTField {{{2
class ASTField:
    def __init__(self, type, name):
        self.type = type
        self.name = name

    def __eq__(self, other):
        return self.type == other.type and self.name == other.name
# Classes to generate {{{1
def list_ast(name, type_, field_name):
    return AST(name, 'ListB', f'std::vector<std::unique_ptr<{type_}>>|{field_name}')
asts = [
    ASTSuperBase(),
    ASTBase('CUB'),
    ASTBase('Decl'),
    ASTBase('ImplMember'),
    ASTBase('Stmt'),
    ASTBase('Expr'),
    ASTBase('Type'),
    ASTBase('ArgB'),
    ASTBase('ParamB'),
    ASTBase('VStmtIB'),
    ASTBase('PathB'),

    ASTBase('ListB'),
    list_ast('DeclList', 'Decl', 'decls'),
    list_ast('StmtList', 'Stmt', 'stmts'),
    list_ast('ParamList', 'ParamB', 'params'),
    list_ast('ArgList', 'Arg', 'args'),
    list_ast('VarStmtItemList', 'VarStmtItem', 'items'),
    list_ast('ImplMemberList', 'ImplMember', 'members'),

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
    AST('ImplDecl'          , 'Decl', 'std::unique_ptr<Type>|implFor, std::vector<std::unique_ptr<ImplMember>>|members'),
    AST('FunctionDecl'      , 'Decl', 'std::unique_ptr<Type>|retty, Token|name, std::vector<std::unique_ptr<ParamB>>|params, std::unique_ptr<Block>|body'),

    AST('FunctionImplMember', 'ImplMember', 'std::unique_ptr<FunctionDecl>|fun'),

    AST('VarStmt'           , 'Stmt', 'std::vector<std::unique_ptr<VarStmtItem>>|items'),
    AST('VarStmtItem'       , 'VStmtIB', 'std::unique_ptr<Type>|type, bool|mut, Token|name, Token|equal, std::unique_ptr<Expr>|expr'),

    AST('ExprStmt'          , 'Stmt', 'std::unique_ptr<Expr>|expr, bool|suppress, Maybe<Location const>|dot'),
    AST('RetStmt'           , 'Stmt', 'std::unique_ptr<Expr>|expr'),

    AST('PathType'          , 'Type', 'std::unique_ptr<Path>|path'),
    AST('PointerType'       , 'Type', 'bool|mut, std::unique_ptr<Type>|type'),
    AST('ThisType'          , 'Type', 'Token|th'),

    AST('Arg'               , 'ArgB', 'std::unique_ptr<Expr>|expr'),

    AST('Param'             , 'ParamB', 'std::unique_ptr<Type>|type, Token|name, bool|mut'),
    AST('ThisParam'         , 'ParamB', 'bool|ptr, bool|mut'),

    AST('Block'             , 'Expr', 'std::vector<std::unique_ptr<Stmt>>|stmts'),
    AST('IfExpr'            , 'Expr', 'Token|iftok, Token|elsetok, std::unique_ptr<Expr>|cond, std::unique_ptr<Expr>|trues, std::unique_ptr<Expr>|falses'),
    AST('WhileExpr'         , 'Expr', 'std::unique_ptr<Expr>|cond, std::unique_ptr<Expr>|body'),

    AST('AssignmentExpr'    , 'Expr', 'std::unique_ptr<Expr>|target, Token|equal, std::unique_ptr<Expr>|expr'),
    AST('ShortCircuitExpr'  , 'Expr', 'std::unique_ptr<Expr>|lhs, Token|op, std::unique_ptr<Expr>|rhs'),
    AST('BinaryExpr'        , 'Expr', 'std::unique_ptr<Expr>|lhs, Token|op, std::unique_ptr<Expr>|rhs'),
    AST('CastExpr'          , 'Expr', 'std::unique_ptr<Type>|type, std::unique_ptr<Expr>|expr'),
    AST('UnaryExpr'         , 'Expr', 'Token|op, std::unique_ptr<Expr>|expr'),
    AST('AddrofExpr'        , 'Expr', 'Token|op, std::unique_ptr<Expr>|expr, bool|mut'),
    AST('DerefExpr'         , 'Expr', 'Token|op, std::unique_ptr<Expr>|expr'),
    AST('CallExpr'          , 'Expr', 'std::unique_ptr<Expr>|callee, Token|oparn, std::vector<std::unique_ptr<Arg>>|args'),
    AST('FieldAccessExpr'   , 'Expr', 'std::unique_ptr<Expr>|operand, Token|dot, Token|field'),
    AST('MethodCallExpr'    , 'Expr', 'std::unique_ptr<Expr>|operand, Token|dot, Token|method, Token|oparn, std::vector<std::unique_ptr<Arg>>|args'),
    AST('PrimaryExpr'       , 'Expr', 'Token|value'),
    AST('PathExpr'          , 'Expr', 'std::unique_ptr<Path>|path'),

    AST('Path'              , 'PathB', 'std::vector<Token>|segments'),
]
# Generating methods {{{1
# Generating AST stuff {{{2
# Generate AST declarations {{{3
def gen_ast_decls():
    output = []
    for ast in asts:
        if isinstance(ast, AST):
            output.append(f'    class {ast.name} : public {ast.base} {{\n')

            output.append('    public:\n')
            output.append('        Maybe<Location const> _start, _end;\n')

            for field in ast.fields:
                output.append(f'        {field.type} {field.name};\n')

            output.append(f'        virtual void accept({ast.base}Visitor &v) override;\n')
            output.append( '        virtual Maybe<Location const> & start() override;\n')
            output.append( '        virtual Maybe<Location const> & end() override;\n')
            output.append(f'        {ast.name}(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, {", ".join(f"{field.type} {field.name}" for field in ast.fields)});\n')

            output.append('    };\n')
        elif isinstance(ast, ASTBase):
            output.append(f'    class {ast.name} : public AST {{\n')
            output.append('    public:\n')


            output.append(f'        virtual ~{ast.name}() {{}}\n')
            output.append(f'        virtual void accept({ast.name}Visitor &v) = 0;\n')
            output.append(f'        {ast.name}(File const &file);\n')
            output.append('    };\n')
        else:
            output.append('    class AST {\n')
            output.append('    public:\n')
            output.append('        AST(File const &file);\n')
            output.append('        virtual ~AST() {}\n')
            output.append('        virtual Maybe<Location const> & start() = 0;\n')
            output.append('        virtual Maybe<Location const> & end() = 0;\n')
            output.append('        File const &file;\n')
            output.append('    };\n')

    return''.join(output)
# Generate AST definitions {{{3
def gen_ast_defs():
    output = []
    for ast in asts:
        if isinstance(ast, AST):
            output.append(f'ASTNS::{ast.name}::{ast.name}(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, {", ".join(f"{field.type} {field.name}" for field in ast.fields)}): ')

            init_list = [f'{ast.base}(file), _start(start), _end(end)']
            for field in ast.fields:
                init_list.append(f'{field.name}(std::move({field.name}))')

            output.append(', '.join(init_list))
            output.append(' {}\n')

            output.append(f'void ASTNS::{ast.name}::accept(ASTNS::{ast.base}Visitor &v) {{ v.visit{ast.name}(*this); }}\n')
            output.append(f'Maybe<Location const> & ASTNS::{ast.name}::start() {{ return _start; }}\n')
            output.append(f'Maybe<Location const> & ASTNS::{ast.name}::end() {{ return _end; }}\n')
        elif isinstance(ast, ASTBase):
            output.append(f'ASTNS::{ast.name}::{ast.name}(File const &file): AST(file) {{}}\n')
        else:
            output.append('ASTNS::AST::AST(File const &file): file(file) {}\n')

    return ''.join(output)
# Generate AST forward decls {{{3
def gen_ast_fwd():
    output = []
    for ast in asts: output.append(f'    class {ast.name};\n')
    return ''.join(output)
# Generating Visitor stuff {{{2
# Generate Visitor decls {{{3
def gen_visitor_decls():
    output = []
    for ast in asts:
        if isinstance(ast, ASTBase):
            output.append(f'    class {ast.name}Visitor {{\n')
            output.append( '    public:\n')
            output.append(f'        virtual ~{ast.name}Visitor() {{}}\n')
            for _ast in asts:
                if isinstance(_ast, AST) and _ast.base == ast.name:
                    output.append(f'        virtual void visit{_ast.name}(ASTNS::{_ast.name} &ast) = 0;\n')
            output.append('    };\n')
    return ''.join(output)
# Generate overrided functions for visitor classes {{{3
def gen_visitor_methods(*bases):
    output = []
    for ast in asts:
        if not isinstance(ast, AST):
            continue

        if (ast.base in bases or bases == ('all',)):
            output.append(f'void visit{ast.name}(ASTNS::{ast.name} &ast) override;\n')

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

        output.append(          f'void ASTNS::PrintVisitor::visit{ast.name}(ASTNS::{ast.name} &a) {{\n')
        output.append(          f'    pai("{ast.name} {{\\n");\n')
        output.append(           '    ++indent;\n')
        for field in ast.fields:
            output.append(      f'    pai("{field.type} {field.name} = ");\n')
            output.append(      f'    printField(*this, a.{field.name});\n')
        output.append(           '    --indent;\n')
        output.append(           '    pai("}\\n");\n')
        output.append(           '}\n')

    return''.join(output)
