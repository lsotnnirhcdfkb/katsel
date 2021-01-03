#!/usr/bin/env python3

# Classes {{{1
# AST {{{2
class AST:
    def __init__(self, name, base, fields):
        self.name = name
        def process_field(field):
            return (item.strip().rstrip() for item in field.split('|'))
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
asts = [
    ASTSuperBase(),
    ASTBase('CUB'),
    ASTBase('Decl'),
    ASTBase('Stmt'),
    ASTBase('Expr'),
    ASTBase('Type'),
    ASTBase('ArgB'),
    ASTBase('ParamB'),
    ASTBase('VStmtIB'),
    ASTBase('ImplRetB'),
    ASTBase('PathB'),

    # a class to keep track of locations of syntactic elements that don't matter (like
    # line endings where the location matters (so that other statements can use the line
    # ending as an ending position) but does not reduce down to an actual AST,
    # so its location cannot be kept track of otherwise)
    # it should never appear in the final AST, because no fields should have a PureLocationB
    # or a PureLocation as a field
    ASTBase('PureLocationB'),
    AST('PureLocation',  'PureLocationB', 'int|dummy'),

    AST('CU'               , 'CUB', 'std::unique_ptr<DeclList>|decls'),
    AST('DeclList'         , 'Decl', 'std::vector<std::unique_ptr<Decl>>|decls'),
    # AST('ImplDecl'         , 'Decl', 'std::unique_ptr<Type>|implfor'),
    AST('ImplicitDecl'     , 'Decl', 'int|dummy'),
    AST('FunctionDecl'     , 'Decl', 'std::unique_ptr<Type>|retty, Token|name, std::unique_ptr<ParamList>|params, std::unique_ptr<Block>|body'),
    AST('VarStmt'          , 'Stmt', 'std::unique_ptr<VarStmtItemList>|assignments'),
    AST('VarStmtItem'      , 'VStmtIB', 'std::unique_ptr<Type>|type, Token|name, Token|equal, std::unique_ptr<Expr>|expr'),
    AST('VarStmtItemList'  , 'VStmtIB', 'std::vector<std::unique_ptr<VarStmtItem>>|items'),
    AST('ExprStmt'         , 'Stmt', 'std::unique_ptr<Expr>|expr'),
    AST('RetStmt'          , 'Stmt', 'std::unique_ptr<Expr>|expr'),
    AST('StmtList'         , 'Stmt', 'std::vector<std::unique_ptr<Stmt>>|stmts'),
    AST('ImplRet'          , 'ImplRetB', 'std::unique_ptr<Expr>|expr'),
    AST('PathType'         , 'Type', 'std::unique_ptr<Path>|path'),
    AST('PointerType'      , 'Type', 'std::unique_ptr<Type>|type'),
    AST('Arg'              , 'ArgB', 'std::unique_ptr<Expr>|expr'),
    AST('ArgList'          , 'ArgB', 'std::vector<std::unique_ptr<Arg>>|args'),
    AST('Param'            , 'ParamB', 'std::unique_ptr<Type>|type, Token|name'),
    AST('ParamList'        , 'ParamB', 'std::vector<std::unique_ptr<Param>>|params'),
    AST('Block'            , 'Expr', 'std::unique_ptr<StmtList>|stmts, std::unique_ptr<ImplRet>|implRet'),
    AST('IfExpr'           , 'Expr', 'Token|iftok, std::unique_ptr<Expr>|cond, std::unique_ptr<Expr>|trues, std::unique_ptr<Expr>|falses'),
    AST('ForExpr'          , 'Expr', 'std::unique_ptr<VarStmt>|initial, std::unique_ptr<Expr>|cond, std::unique_ptr<Expr>|increment, std::unique_ptr<Expr>|body'),
    AST('AssignmentExpr'   , 'Expr', 'std::unique_ptr<Expr>|target, Token|equal, std::unique_ptr<Expr>|expr'),
    AST('ShortCircuitExpr' , 'Expr', 'std::unique_ptr<Expr>|lhs, Token|op, std::unique_ptr<Expr>|rhs'),
    AST('BinaryExpr'       , 'Expr', 'std::unique_ptr<Expr>|lhs, Token|op, std::unique_ptr<Expr>|rhs'),
    AST('CastExpr'         , 'Expr', 'std::unique_ptr<Type>|type, std::unique_ptr<Expr>|expr'),
    AST('UnaryExpr'        , 'Expr', 'Token|op, std::unique_ptr<Expr>|expr'),
    AST('AddrofExpr'       , 'Expr', 'Token|op, std::unique_ptr<Expr>|expr'),
    AST('DerefExpr'        , 'Expr', 'Token|op, std::unique_ptr<Expr>|expr'),
    AST('CallExpr'         , 'Expr', 'std::unique_ptr<Expr>|callee, Token|oparn, std::unique_ptr<ArgList>|args'),
    AST('PrimaryExpr'      , 'Expr', 'Token|value'),
    AST('PathExpr'         , 'Expr', 'std::unique_ptr<Path>|path'),
    AST('Path'             , 'PathB', 'std::vector<Token>|segments'),
]
# Generating methods {{{1
# Generating AST stuff {{{2
# Generate AST declarations {{{3
def gen_ast_decls():
    output = []
    for ast in asts:
        output.append(f'    class {ast.name};\n')

    for ast in asts:
        if isinstance(ast, AST):
            output.append(f'    class {ast.name} : public {ast.base} {{\n')

            output.append('    public:\n')
            output.append('        Location _start, _end;\n')

            for field in ast.fields:
                output.append(f'        {field.type} {field.name};\n')

            output.append(f'        virtual void accept(ASTNS::{ast.base}::Visitor *v) override;\n')
            output.append( '        virtual Location const & start() override;\n')
            output.append( '        virtual Location const & end() override;\n')
            output.append(f'        {ast.name}(File const &file, Location start, Location end, {", ".join(f"{field.type} {field.name}" for field in ast.fields)});\n')

            output.append('    };\n')
        elif isinstance(ast, ASTBase):
            output.append(f'    class {ast.name} : public AST {{\n')
            output.append('    public:\n')

            output.append('        class Visitor {\n')
            output.append('        public:\n')
            output.append('            virtual ~Visitor() {}\n')
            for _ast in asts:
                if isinstance(_ast, AST) and _ast.base == ast.name:
                    output.append(f'            virtual void visit{_ast.name}(ASTNS::{_ast.name} *ast) = 0;\n')
            output.append('        };\n')

            output.append(f'        virtual ~{ast.name}() {{}}\n')
            output.append('        virtual void accept(Visitor *v) = 0;\n')
            output.append(f'        {ast.name}(File const &file);\n')
            output.append('    };\n')
        else:
            output.append('    class AST {\n')
            output.append('    public:\n')
            output.append('        AST(File const &file);\n')
            output.append('        virtual ~AST() {}\n')
            output.append('        virtual Location const & start() = 0;\n')
            output.append('        virtual Location const & end() = 0;\n')
            output.append('        File const &file;\n')
            output.append('    };\n')

    return''.join(output)
# Generate AST definitions {{{3
def gen_ast_defs():
    output = []
    for ast in asts:
        if isinstance(ast, AST):
            output.append(f'ASTNS::{ast.name}::{ast.name}(File const &file, Location start, Location end, {", ".join(f"{field.type} {field.name}" for field in ast.fields)}): ')

            init_list = [f'{ast.base}(file), _start(start), _end(end)']
            for field in ast.fields:
                init_list.append(f'{field.name}(std::move({field.name}))')

            output.append(', '.join(init_list))
            output.append(' {}\n')

            output.append(f'void ASTNS::{ast.name}::accept(ASTNS::{ast.base}::Visitor *v) {{ v->visit{ast.name}(this); }}\n')
            output.append(f'Location const & ASTNS::{ast.name}::start() {{ return _start; }}\n')
            output.append(f'Location const & ASTNS::{ast.name}::end() {{ return _end; }}\n')
        elif isinstance(ast, ASTBase):
            output.append(f'ASTNS::{ast.name}::{ast.name}(File const &file): AST(file) {{}}\n')
        else:
            output.append('ASTNS::AST::AST(File const &file): file(file) {}\n')

    return''.join(output)
# Generating Visitor stuff {{{2
# Generate overrided functions for visitor classes {{{3
def gen_visitor_methods(*bases):
    output = []
    for ast in asts:
        if not isinstance(ast, AST):
            continue

        if (ast.base in bases or bases == ('all',)):
            output.append(f'void visit{ast.name}(ASTNS::{ast.name} *ast) override;\n')

    return''.join(output)
# Generate inheriting classes {{{3
def gen_visitor_inherit_all():
    return ',\n'.join([f'public ASTNS::{cl.name}::Visitor' for cl in asts if isinstance(cl, ASTBase)]) + '\n'
# Generating printing stuff {{{2
# Genearte print visitor {{{3
def gen_print_visitor_methods():
    output = []
    for ast in asts:
        if not isinstance(ast, AST):
            continue

        output.append(          f'void ASTNS::PrintVisitor::visit{ast.name}(ASTNS::{ast.name} *a) {{\n')
        output.append(          f'    pai("{ast.name} {{\\n");\n')
        output.append(           '    ++indent;\n')
        for field in ast.fields:
            output.append(      f'    pai("{field.type} {field.name} = ");\n')
            output.append(      f'    printField(a->{field.name});\n')
        output.append(           '    --indent;\n')
        output.append(           '    pai("}\\n");\n')
        output.append(           '}\n')

    return''.join(output)
