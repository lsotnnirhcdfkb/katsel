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
    void visit(ASTNS::ImplicitDecl &ast) override;
    void visit(ASTNS::CU &ast) override;
    void visit(ASTNS::ImplDecl &ast) override;
    void visit(ASTNS::FunctionDecl &ast) override;
    // FORWDECL METHODS END

    CodeGen &cg;
};
// Declarator {{{1
class CodeGen::Declarator : public ASTNS::DeclVisitor, public ASTNS::ImplMemberVisitor, public ASTNS::CUBVisitor {
public:
    Declarator(CodeGen &cg);

private:
    // DECLARATOR METHODS START
    void visit(ASTNS::ImplicitDecl &ast) override;
    void visit(ASTNS::CU &ast) override;
    void visit(ASTNS::ImplDecl &ast) override;
    void visit(ASTNS::FunctionDecl &ast) override;
    void visit(ASTNS::FunctionImplMember &ast) override;
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

        void stmt(ASTNS::Stmt &ast);

    private:
        // STMTCG METHODS START
        void visit(ASTNS::VarStmt &ast) override;
        void visit(ASTNS::VarStmtItem &ast) override;
        void visit(ASTNS::ExprStmt &ast) override;
        void visit(ASTNS::RetStmt &ast) override;
        // STMTCG METHODS END

        CodeGen &cg;
        FunctionCodeGen &fcg;
    };
    // }}}
    // ExprCodeGen {{{
    class ExprCodeGen : public ASTNS::ExprVisitor {
    public:
        ExprCodeGen(CodeGen &cg, FunctionCodeGen &fcg);

        Maybe<IR::ASTValue> expr(ASTNS::Expr &ast);

    private:
        // EXPRCG METHODS START
        void visit(ASTNS::Block &ast) override;
        void visit(ASTNS::IfExpr &ast) override;
        void visit(ASTNS::WhileExpr &ast) override;
        void visit(ASTNS::AssignmentExpr &ast) override;
        void visit(ASTNS::ShortCircuitExpr &ast) override;
        void visit(ASTNS::BinaryExpr &ast) override;
        void visit(ASTNS::CastExpr &ast) override;
        void visit(ASTNS::UnaryExpr &ast) override;
        void visit(ASTNS::AddrofExpr &ast) override;
        void visit(ASTNS::DerefExpr &ast) override;
        void visit(ASTNS::CallExpr &ast) override;
        void visit(ASTNS::FieldAccessExpr &ast) override;
        void visit(ASTNS::MethodCallExpr &ast) override;
        void visit(ASTNS::PrimaryExpr &ast) override;
        void visit(ASTNS::PathExpr &ast) override;
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

    void add_local(std::string const &name, IR::Instrs::Register &val);
    Maybe<Local&> get_local(std::string const &name);

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
    void visit(ASTNS::FunctionImplMember &ast) override;
    // IMPLCG METHODS END

    CodeGen &cg;
    NNPtr<ASTNS::ImplDecl const> ast;

    Maybe<NNPtr<IR::Type>> impl_for;

    bool errored;
};
// Param and Args {{{1
// Param {{{2
class CodeGen::ParamVisitor : public ASTNS::ParamBVisitor {
public:
    struct Param {
        NNPtr<IR::Type const> ty;
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
    void visit(ASTNS::Param &ast) override;
    void visit(ASTNS::ThisParam &ast) override;
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
    void visit(ASTNS::Arg &ast) override;
    // ARGSVISITOR METHODS END

    CodeGen::FunctionCodeGen &fcg;
};
// Path {{{1
class CodeGen::PathVisitor : public ASTNS::PathBVisitor {
public:
    PathVisitor(CodeGen &cg);

    Maybe<IR::ASTValue> resolve_value(ASTNS::PathB &path, CodeGen::FunctionCodeGen &fcg);
    Maybe<IR::DeclSymbol &> resolve_decl_symbol(ASTNS::PathB &path);

private:
    enum class PathType { VALUE, DECLARED } pty;

    Maybe<IR::ASTValue> vret;
    Maybe<NNPtr<IR::DeclSymbol>> dret;

    // PATH VISITOR START
    void visit(ASTNS::Path &ast) override;
    // PATH VISITOR END

    CodeGen &cg;
    Maybe<NNPtr<FunctionCodeGen>> fcg;
};
// TypeVisitor {{{1
class CodeGen::TypeVisitor : public ASTNS::TypeVisitor {
public:
    TypeVisitor(CodeGen &cg);

    Maybe<IR::Type &> type(ASTNS::Type &ast, Maybe<NNPtr<IR::Type>> this_type);

private:
    // TYPEVISITOR METHODS START
    void visit(ASTNS::PathType &ast) override;
    void visit(ASTNS::PointerType &ast) override;
    void visit(ASTNS::ThisType &ast) override;
    // TYPEVISITOR METHODS END

    Maybe<NNPtr<IR::Type>> ret;
    Maybe<NNPtr<IR::Type>> this_type;

    CodeGen &cg;
};

