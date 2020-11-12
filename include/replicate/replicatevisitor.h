#pragma once

#include "visit/visitor.h"
#include "parse/ast.h"
#include "lex/token.h"

class ReplicateVisitor : public ASTVisitor
{
public:
    // REPLICATE METHODS START

// The following code was autogenerated - see the utils/ directory
void visitAdditionexpr(ASTNS::Additionexpr *ast) override;
void visitArgs(ASTNS::Args *ast) override;
void visitAssignmentexpr(ASTNS::Assignmentexpr *ast) override;
void visitBinandexpr(ASTNS::Binandexpr *ast) override;
void visitBinnotexpr(ASTNS::Binnotexpr *ast) override;
void visitBinorexpr(ASTNS::Binorexpr *ast) override;
void visitBitandexpr(ASTNS::Bitandexpr *ast) override;
void visitBitorexpr(ASTNS::Bitorexpr *ast) override;
void visitBitshiftexpr(ASTNS::Bitshiftexpr *ast) override;
void visitBitxorexpr(ASTNS::Bitxorexpr *ast) override;
void visitBlock(ASTNS::Block *ast) override;
void visitCallexpr(ASTNS::Callexpr *ast) override;
void visitCompeqexpr(ASTNS::Compeqexpr *ast) override;
void visitComplgtexpr(ASTNS::Complgtexpr *ast) override;
void visitDecl(ASTNS::Decl *ast) override;
void visitDecls(ASTNS::Decls *ast) override;
void visitExpression(ASTNS::Expression *ast) override;
void visitExprstmt(ASTNS::Exprstmt *ast) override;
void visitFunction(ASTNS::Function *ast) override;
void visitMultexpr(ASTNS::Multexpr *ast) override;
void visitParamlist(ASTNS::Paramlist *ast) override;
void visitPrimaryexpr(ASTNS::Primaryexpr *ast) override;
void visitRetstmt(ASTNS::Retstmt *ast) override;
void visitStmt(ASTNS::Stmt *ast) override;
void visitStmts(ASTNS::Stmts *ast) override;
void visitTernaryexpr(ASTNS::Ternaryexpr *ast) override;
void visitType(ASTNS::Type *ast) override;
void visitUnaryexpr(ASTNS::Unaryexpr *ast) override;
void visitVarstmt(ASTNS::Varstmt *ast) override;
void visitVarstmtfinisher(ASTNS::Varstmtfinisher *ast) override;
// This code was autogenerated - see the utils/ directory

    // REPLICATE METHODS END

private:
    int indent;
    void pi();
};
