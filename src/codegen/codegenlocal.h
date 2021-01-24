#pragma once

#include "codegen/codegen.h"
#include "codegen/context.h"

#include "ast/ast.h"

#include "ir/value.h"
#include "ir/type.h"
#include "ir/instruction.h"
#include "ir/module.h"

// ForwDecl {{{1
class CodeGen::ForwDecl : public ASTNS::DeclVisitor, public ASTNS::CUBVisitor {
public:
    ForwDecl(CodeGen &cg);

private:
    // FORWDECL METHODS START
// The following code was autogenerated - see the utils/ directory
void visitImplicitDecl(ASTNS::ImplicitDecl &ast) override;
void visitCU(ASTNS::CU &ast) override;
void visitImplDecl(ASTNS::ImplDecl &ast) override;
void visitFunctionDecl(ASTNS::FunctionDecl &ast) override;
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
void visitImplicitDecl(ASTNS::ImplicitDecl &ast) override;
void visitCU(ASTNS::CU &ast) override;
void visitImplDecl(ASTNS::ImplDecl &ast) override;
void visitFunctionDecl(ASTNS::FunctionDecl &ast) override;
void visitFunctionImplMember(ASTNS::FunctionImplMember &ast) override;
// This code was autogenerated - see the utils/ directory
    // DECLARATOR METHODS END

    CodeGen &cg;

    NNPtr<IR::DeclSymbol> currentSymbol;
    Maybe<NNPtr<IR::Type>> thisType;
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
void visitVarStmt(ASTNS::VarStmt &ast) override;
void visitVarStmtItem(ASTNS::VarStmtItem &ast) override;
void visitExprStmt(ASTNS::ExprStmt &ast) override;
void visitRetStmt(ASTNS::RetStmt &ast) override;
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
void visitBlock(ASTNS::Block &ast) override;
void visitIfExpr(ASTNS::IfExpr &ast) override;
void visitWhileExpr(ASTNS::WhileExpr &ast) override;
void visitAssignmentExpr(ASTNS::AssignmentExpr &ast) override;
void visitShortCircuitExpr(ASTNS::ShortCircuitExpr &ast) override;
void visitBinaryExpr(ASTNS::BinaryExpr &ast) override;
void visitCastExpr(ASTNS::CastExpr &ast) override;
void visitUnaryExpr(ASTNS::UnaryExpr &ast) override;
void visitAddrofExpr(ASTNS::AddrofExpr &ast) override;
void visitDerefExpr(ASTNS::DerefExpr &ast) override;
void visitCallExpr(ASTNS::CallExpr &ast) override;
void visitFieldAccessExpr(ASTNS::FieldAccessExpr &ast) override;
void visitMethodCallExpr(ASTNS::MethodCallExpr &ast) override;
void visitPrimaryExpr(ASTNS::PrimaryExpr &ast) override;
void visitPathExpr(ASTNS::PathExpr &ast) override;
// This code was autogenerated - see the utils/ directory
        // EXPRCG METHODS END

        Maybe<IR::ASTValue> ret;
        CodeGen &cg;
        FunctionCodeGen &fcg;
    };
    // }}}
public:
    FunctionCodeGen(CodeGen &cg, NNPtr<ASTNS::FunctionDecl> ast, NNPtr<IR::Function> fun, Maybe<NNPtr<IR::Type>> thisType);

    bool codegen();

    struct Local {
        size_t scopenum;
        NNPtr<IR::Instrs::Register> v;
        std::string name;
    };

    std::vector<Local> locals;
    size_t curScope;

    void addLocal(std::string const &name, NNPtr<IR::Instrs::Register> val);
    Maybe<NNPtr<Local>> getLocal(std::string const &name);

    void incScope();
    void decScope();

    CodeGen &cg;
    NNPtr<ASTNS::FunctionDecl> ast;

    ExprCodeGen exprCG;
    StmtCodeGen stmtCG;

    NNPtr<IR::Function> fun;
    NNPtr<IR::Block> registerBlock;
    NNPtr<IR::Block> entryBlock;
    NNPtr<IR::Block> exitBlock;
    NNPtr<IR::Block> curBlock;
    NNPtr<IR::Instrs::Register> ret;

    Maybe<NNPtr<IR::Type>> thisType;

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
void visitFunctionImplMember(ASTNS::FunctionImplMember &ast) override;
// This code was autogenerated - see the utils/ directory
    // IMPLCG METHODS END

    CodeGen &cg;
    NNPtr<ASTNS::ImplDecl> ast;

    Maybe<NNPtr<IR::Type>> implFor;

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

    ParamVisitor(CodeGen &cg, std::vector<std::unique_ptr<ASTNS::ParamB>> &params, Maybe<NNPtr<IR::Type>> thisType);

    std::vector<Param> ret;

    bool errored;

    bool isMethod, thisPtr, thisMut;

private:
    // PARAMVISITOR METHODS START
// The following code was autogenerated - see the utils/ directory
void visitParam(ASTNS::Param &ast) override;
void visitThisParam(ASTNS::ThisParam &ast) override;
// This code was autogenerated - see the utils/ directory
    // PARAMVISITOR METHODS END

    CodeGen &cg;
    Maybe<NNPtr<IR::Type>> thisType;
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
void visitArg(ASTNS::Arg &ast) override;
// This code was autogenerated - see the utils/ directory
    // ARGSVISITOR METHODS END

    CodeGen::FunctionCodeGen &fcg;
};
// Path {{{1
class CodeGen::PathVisitor : public ASTNS::PathBVisitor {
public:
    PathVisitor(CodeGen &cg);

    Maybe<IR::ASTValue> resolveValue(NNPtr<ASTNS::PathB> path, CodeGen::FunctionCodeGen &fcg);
    Maybe<NNPtr<IR::DeclSymbol>> resolveDeclSymbol(NNPtr<ASTNS::PathB> path);

private:
    enum class PathType { VALUE, DECLARED } pty;

    Maybe<IR::ASTValue> vret;
    Maybe<NNPtr<IR::DeclSymbol>> dret;

    // PATH VISITOR START
// The following code was autogenerated - see the utils/ directory
void visitPath(ASTNS::Path &ast) override;
// This code was autogenerated - see the utils/ directory
    // PATH VISITOR END

    CodeGen &cg;
    Maybe<NNPtr<FunctionCodeGen>> fcg;
};
// TypeVisitor {{{1
class CodeGen::TypeVisitor : public ASTNS::TypeVisitor {
public:
    TypeVisitor(CodeGen &cg);

    Maybe<NNPtr<IR::Type>> type(NNPtr<ASTNS::Type> ast, Maybe<NNPtr<IR::Type>> thisType);

private:
    // TYPEVISITOR METHODS START
// The following code was autogenerated - see the utils/ directory
void visitPathType(ASTNS::PathType &ast) override;
void visitPointerType(ASTNS::PointerType &ast) override;
void visitThisType(ASTNS::ThisType &ast) override;
// This code was autogenerated - see the utils/ directory
    // TYPEVISITOR METHODS END

    Maybe<NNPtr<IR::Type>> ret;
    Maybe<NNPtr<IR::Type>> thisType;

    CodeGen &cg;
};

