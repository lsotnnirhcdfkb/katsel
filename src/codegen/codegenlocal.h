#pragma once

#include "codegen/codegen.h"
#include "codegen/context.h"

#include "ast/ast.h"

#include "ir/value.h"
#include "ir/type.h"
#include "ir/instruction.h"

// TypeVisitor {{{1
class CodeGen::TypeVisitor : public ASTNS::TypeB::Visitor
{
public:
    TypeVisitor(CodeGen &cg);

    IR::Type* type(ASTNS::TypeB *ast);

private:
    // TYPEVISITOR METHODS START
// The following code was autogenerated - see the utils/ directory
void visitPrimitiveType(ASTNS::PrimitiveType *ast) override;
// This code was autogenerated - see the utils/ directory
    // TYPEVISITOR METHODS END

    IR::Type *ret;

    CodeGen &cg;
};

// ForwDecl {{{1
class CodeGen::ForwDecl : public ASTNS::DeclB::Visitor, public ASTNS::CUB::Visitor
{
public:
    ForwDecl(CodeGen &cg);

private:
    // FORWDECL METHODS START
// The following code was autogenerated - see the utils/ directory
void visitCU(ASTNS::CU *ast) override;
void visitFunctionDecl(ASTNS::FunctionDecl *ast) override;
// This code was autogenerated - see the utils/ directory
    // FORWDECL METHODS END

    CodeGen &cg;
};

// Decls {{{1
// Function {{{2
class CodeGen::FunctionCodeGen
{
    // StmtCodeGen {{{
    class StmtCodeGen : public ASTNS::StmtB::Visitor, public ASTNS::VStmtIB::Visitor
    {
    public:
        StmtCodeGen(CodeGen &cg, FunctionCodeGen &fcg);

        void stmt(ASTNS::StmtB *ast);

    private:
        // STMTCG METHODS START
// The following code was autogenerated - see the utils/ directory
void visitVarStmt(ASTNS::VarStmt *ast) override;
void visitVarStmtItem(ASTNS::VarStmtItem *ast) override;
void visitExprStmt(ASTNS::ExprStmt *ast) override;
void visitRetStmt(ASTNS::RetStmt *ast) override;
void visitBlock(ASTNS::Block *ast) override;
// This code was autogenerated - see the utils/ directory
        // STMTCG METHODS END

        CodeGen &cg;
        FunctionCodeGen &fcg;

    private:
        IR::Type *varty;
    };
    // }}}
    // ExprCodeGen {{{
    class ExprCodeGen : public ASTNS::ExprB::Visitor
    {
    public:
        ExprCodeGen(CodeGen &cg, FunctionCodeGen &fcg);

        IR::ASTValue expr(ASTNS::ExprB *ast);

    private:
        // EXPRCG METHODS START
// The following code was autogenerated - see the utils/ directory
void visitIfExpr(ASTNS::IfExpr *ast) override;
void visitForExpr(ASTNS::ForExpr *ast) override;
void visitAssignmentExpr(ASTNS::AssignmentExpr *ast) override;
void visitShortCircuitExpr(ASTNS::ShortCircuitExpr *ast) override;
void visitBinaryExpr(ASTNS::BinaryExpr *ast) override;
void visitCastExpr(ASTNS::CastExpr *ast) override;
void visitUnaryExpr(ASTNS::UnaryExpr *ast) override;
void visitCallExpr(ASTNS::CallExpr *ast) override;
void visitPrimaryExpr(ASTNS::PrimaryExpr *ast) override;
// This code was autogenerated - see the utils/ directory
        // EXPRCG METHODS END

        IR::ASTValue ret;
        CodeGen &cg;
        FunctionCodeGen &fcg;
    };
    // }}}
public:
    FunctionCodeGen(CodeGen &cg, ASTNS::FunctionDecl *ast);

    bool codegen();

    struct Local
    {
        size_t scopenum;
        IR::Register *v;
        std::string name;
    };

    std::vector<Local> locals;
    size_t curScope;

    void addLocal(std::string const &name, IR::Register *val);
    Local* getLocal(std::string const &name);

    void incScope();
    void decScope();

    CodeGen &cg;
    ASTNS::FunctionDecl *ast;

    ExprCodeGen exprCG;
    StmtCodeGen stmtCG;

    IR::Function *fun;
    IR::Register *ret;
    IR::Block *curBlock;
    IR::Block *exitBlock;

    bool errored;
};

// Param and Args {{{1
// Param {{{2
class CodeGen::ParamVisitor : public ASTNS::PListB::Visitor
{
public:
    struct Param
    {
        IR::Type *ty;
        std::string name;
        ASTNS::Param *ast;
    };

    ParamVisitor(CodeGen &cg);

    std::vector<Param> ret;

private:
    // PARAMVISITOR METHODS START
// The following code was autogenerated - see the utils/ directory
void visitParam(ASTNS::Param *ast) override;
// This code was autogenerated - see the utils/ directory
    // PARAMVISITOR METHODS END

    CodeGen &cg;
};

// Arg {{{2
class CodeGen::ArgVisitor : public ASTNS::ArgB::Visitor
{
public:
    ArgVisitor(CodeGen::FunctionCodeGen &fcg);
    std::vector<IR::ASTValue> ret;

private:
    // ARGSVISITOR METHODS START
// The following code was autogenerated - see the utils/ directory
void visitArg(ASTNS::Arg *ast) override;
// This code was autogenerated - see the utils/ directory
    // ARGSVISITOR METHODS END

    CodeGen::FunctionCodeGen &fcg;
};
