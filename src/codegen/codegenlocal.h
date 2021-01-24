#pragma once

#include "codegen/codegen.h"
#include "codegen/context.h"

#include "ast/astfwd.h"

#include "ir/value.h"
#include "ir/type.h"
#include "ir/instructionfwd.h"
#include "ir/module.h"

// ForwDecl {{{1
class CodeGen::ForwDecl : public ASTNS::DeclVisitor, public ASTNS::CUBVisitor {
public:
    ForwDecl(CodeGen &cg);

private:
    // FORWDECL METHODS START
// The following code was autogenerated - see the utils/ directory
void visit_implicit_decl(ASTNS::ImplicitDecl &ast) override;
void visit_cu(ASTNS::CU &ast) override;
void visit_impl_decl(ASTNS::ImplDecl &ast) override;
void visit_function_decl(ASTNS::FunctionDecl &ast) override;
// This code was autogenerated - see the utils/ directory
    // FORWDECL METHODS END

    CodeGen &cg;
};
// Declarator {{{1
class CodeGen::Declarator : public ASTNS::DeclVisitor, public ASTNS::ImplMemberVisitor, public ASTNS::CUBVisitor {
public:
    Declarator(CodeGen &cg);

private:
    // DECLARATOR METHODS START
// The following code was autogenerated - see the utils/ directory
void visit_implicit_decl(ASTNS::ImplicitDecl &ast) override;
void visit_cu(ASTNS::CU &ast) override;
void visit_impl_decl(ASTNS::ImplDecl &ast) override;
void visit_function_decl(ASTNS::FunctionDecl &ast) override;
void visit_function_impl_member(ASTNS::FunctionImplMember &ast) override;
// This code was autogenerated - see the utils/ directory
    // DECLARATOR METHODS END

    CodeGen &cg;

    NNPtr<IR::DeclSymbol> current_symbol;
    Maybe<NNPtr<IR::Type>> this_type;
};

// Decls {{{1
// Function {{{2
class CodeGen::FunctionCodeGen {
    // StmtCodeGen {{{
    class StmtCodeGen : public ASTNS::StmtVisitor, public ASTNS::VStmtIBVisitor {
    public:
        StmtCodeGen(CodeGen &cg, FunctionCodeGen &fcg);

        void stmt(NNPtr<ASTNS::Stmt> ast);

    private:
        // STMTCG METHODS START
// The following code was autogenerated - see the utils/ directory
void visit_var_stmt(ASTNS::VarStmt &ast) override;
void visit_var_stmt_item(ASTNS::VarStmtItem &ast) override;
void visit_expr_stmt(ASTNS::ExprStmt &ast) override;
void visit_ret_stmt(ASTNS::RetStmt &ast) override;
// This code was autogenerated - see the utils/ directory
        // STMTCG METHODS END

        CodeGen &cg;
        FunctionCodeGen &fcg;
    };
    // }}}
    // ExprCodeGen {{{
    class ExprCodeGen : public ASTNS::ExprVisitor {
    public:
        ExprCodeGen(CodeGen &cg, FunctionCodeGen &fcg);

        Maybe<IR::ASTValue> expr(NNPtr<ASTNS::Expr> ast);

    private:
        // EXPRCG METHODS START
// The following code was autogenerated - see the utils/ directory
void visit_block(ASTNS::Block &ast) override;
void visit_if_expr(ASTNS::IfExpr &ast) override;
void visit_while_expr(ASTNS::WhileExpr &ast) override;
void visit_assignment_expr(ASTNS::AssignmentExpr &ast) override;
void visit_short_circuit_expr(ASTNS::ShortCircuitExpr &ast) override;
void visit_binary_expr(ASTNS::BinaryExpr &ast) override;
void visit_cast_expr(ASTNS::CastExpr &ast) override;
void visit_unary_expr(ASTNS::UnaryExpr &ast) override;
void visit_addrof_expr(ASTNS::AddrofExpr &ast) override;
void visit_deref_expr(ASTNS::DerefExpr &ast) override;
void visit_call_expr(ASTNS::CallExpr &ast) override;
void visit_field_access_expr(ASTNS::FieldAccessExpr &ast) override;
void visit_method_call_expr(ASTNS::MethodCallExpr &ast) override;
void visit_primary_expr(ASTNS::PrimaryExpr &ast) override;
void visit_path_expr(ASTNS::PathExpr &ast) override;
// This code was autogenerated - see the utils/ directory
        // EXPRCG METHODS END

        Maybe<IR::ASTValue> ret;
        CodeGen &cg;
        FunctionCodeGen &fcg;
    };
    // }}}
public:
    FunctionCodeGen(CodeGen &cg, NNPtr<ASTNS::FunctionDecl> ast, NNPtr<IR::Function> fun, Maybe<NNPtr<IR::Type>> this_type);

    bool codegen();

    struct Local {
        size_t scopenum;
        NNPtr<IR::Instrs::Register> v;
        std::string name;
    };

    std::vector<Local> locals;
    size_t cur_scope;

    void add_local(std::string const &name, NNPtr<IR::Instrs::Register> val);
    Maybe<NNPtr<Local>> get_local(std::string const &name);

    void inc_scope();
    void dec_scope();

    CodeGen &cg;
    NNPtr<ASTNS::FunctionDecl> ast;

    ExprCodeGen expr_cg;
    StmtCodeGen stmt_cg;

    NNPtr<IR::Function> fun;
    NNPtr<IR::Block> register_block;
    NNPtr<IR::Block> entry_block;
    NNPtr<IR::Block> exit_block;
    NNPtr<IR::Block> cur_block;
    NNPtr<IR::Instrs::Register> ret;

    Maybe<NNPtr<IR::Type>> this_type;

    bool errored;
};
// Impl {{{2
class CodeGen::ImplCodeGen : public ASTNS::ImplMemberVisitor {
public:
    ImplCodeGen(CodeGen &cg, NNPtr<ASTNS::ImplDecl> ast);

    bool codegen();

private:
    // IMPLCG METHODS START
// The following code was autogenerated - see the utils/ directory
void visit_function_impl_member(ASTNS::FunctionImplMember &ast) override;
// This code was autogenerated - see the utils/ directory
    // IMPLCG METHODS END

    CodeGen &cg;
    NNPtr<ASTNS::ImplDecl> ast;

    Maybe<NNPtr<IR::Type>> impl_for;

    bool errored;
};
// Param and Args {{{1
// Param {{{2
class CodeGen::ParamVisitor : public ASTNS::ParamBVisitor {
public:
    struct Param {
        NNPtr<IR::Type> ty;
        std::string name;
        NNPtr<ASTNS::ParamB> ast;
        bool mut;
    };

    ParamVisitor(CodeGen &cg, std::vector<std::unique_ptr<ASTNS::ParamB>> &params, Maybe<NNPtr<IR::Type>> this_type);

    std::vector<Param> ret;

    bool errored;

    bool is_method, this_ptr, this_mut;

private:
    // PARAMVISITOR METHODS START
// The following code was autogenerated - see the utils/ directory
void visit_param(ASTNS::Param &ast) override;
void visit_this_param(ASTNS::ThisParam &ast) override;
// This code was autogenerated - see the utils/ directory
    // PARAMVISITOR METHODS END

    CodeGen &cg;
    Maybe<NNPtr<IR::Type>> this_type;
    int index;
};

// Arg {{{2
class CodeGen::ArgVisitor : public ASTNS::ArgBVisitor {
public:
    ArgVisitor(CodeGen::FunctionCodeGen &fcg, std::vector<std::unique_ptr<ASTNS::Arg>> &args);
    std::vector<IR::ASTValue> ret;

private:
    // ARGSVISITOR METHODS START
// The following code was autogenerated - see the utils/ directory
void visit_arg(ASTNS::Arg &ast) override;
// This code was autogenerated - see the utils/ directory
    // ARGSVISITOR METHODS END

    CodeGen::FunctionCodeGen &fcg;
};
// Path {{{1
class CodeGen::PathVisitor : public ASTNS::PathBVisitor {
public:
    PathVisitor(CodeGen &cg);

    Maybe<IR::ASTValue> resolve_value(NNPtr<ASTNS::PathB> path, CodeGen::FunctionCodeGen &fcg);
    Maybe<NNPtr<IR::DeclSymbol>> resolve_decl_symbol(NNPtr<ASTNS::PathB> path);

private:
    enum class PathType { VALUE, DECLARED } pty;

    Maybe<IR::ASTValue> vret;
    Maybe<NNPtr<IR::DeclSymbol>> dret;

    // PATH VISITOR START
// The following code was autogenerated - see the utils/ directory
void visit_path(ASTNS::Path &ast) override;
// This code was autogenerated - see the utils/ directory
    // PATH VISITOR END

    CodeGen &cg;
    Maybe<NNPtr<FunctionCodeGen>> fcg;
};
// TypeVisitor {{{1
class CodeGen::TypeVisitor : public ASTNS::TypeVisitor {
public:
    TypeVisitor(CodeGen &cg);

    Maybe<NNPtr<IR::Type>> type(NNPtr<ASTNS::Type> ast, Maybe<NNPtr<IR::Type>> this_type);

private:
    // TYPEVISITOR METHODS START
// The following code was autogenerated - see the utils/ directory
void visit_path_type(ASTNS::PathType &ast) override;
void visit_pointer_type(ASTNS::PointerType &ast) override;
void visit_this_type(ASTNS::ThisType &ast) override;
// This code was autogenerated - see the utils/ directory
    // TYPEVISITOR METHODS END

    Maybe<NNPtr<IR::Type>> ret;
    Maybe<NNPtr<IR::Type>> this_type;

    CodeGen &cg;
};

