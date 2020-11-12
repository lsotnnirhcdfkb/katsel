#include "codegen/codegen.h"

CodeGen::CodeGen(CodeGenContext &con): context(con) {}

Value CodeGen::evalExpr(ASTNS::AST *a)
{
    exprRetVal = Value();
    a->accept(this);
    return exprRetVal;
}

Type* CodeGen::evalType(ASTNS::AST *a)
{
    typeRetVal = nullptr;
    a->accept(this);
    return typeRetVal;
}

std::vector<CodeGen::Param> CodeGen::evalParams(ASTNS::AST *a)
{
    
}

void CodeGen::visitAdditionexpr(ASTNS::Additionexpr *ast) {}
void CodeGen::visitArgs(ASTNS::Args *ast) {}
void CodeGen::visitAssignmentexpr(ASTNS::Assignmentexpr *ast) {}
void CodeGen::visitBinandexpr(ASTNS::Binandexpr *ast) {}
void CodeGen::visitBinnotexpr(ASTNS::Binnotexpr *ast) {}
void CodeGen::visitBinorexpr(ASTNS::Binorexpr *ast) {}
void CodeGen::visitBitandexpr(ASTNS::Bitandexpr *ast) {}
void CodeGen::visitBitorexpr(ASTNS::Bitorexpr *ast) {}
void CodeGen::visitBitshiftexpr(ASTNS::Bitshiftexpr *ast) {}
void CodeGen::visitBitxorexpr(ASTNS::Bitxorexpr *ast) {}
void CodeGen::visitBlock(ASTNS::Block *ast) {}
void CodeGen::visitCallexpr(ASTNS::Callexpr *ast) {}
void CodeGen::visitCompeqexpr(ASTNS::Compeqexpr *ast) {}
void CodeGen::visitComplgtexpr(ASTNS::Complgtexpr *ast) {}
void CodeGen::visitDecl(ASTNS::Decl *ast) {}
void CodeGen::visitDecls(ASTNS::Decls *ast) {}
void CodeGen::visitExpression(ASTNS::Expression *ast) {}
void CodeGen::visitExprstmt(ASTNS::Exprstmt *ast) {}
void CodeGen::visitFunction(ASTNS::Function *ast) {}
void CodeGen::visitMultexpr(ASTNS::Multexpr *ast) {}
void CodeGen::visitParamlist(ASTNS::Paramlist *ast) {}
void CodeGen::visitPrimaryexpr(ASTNS::Primaryexpr *ast) {}
void CodeGen::visitRetstmt(ASTNS::Retstmt *ast) {}
void CodeGen::visitStmt(ASTNS::Stmt *ast) {}
void CodeGen::visitStmts(ASTNS::Stmts *ast) {}
void CodeGen::visitTernaryexpr(ASTNS::Ternaryexpr *ast) {}
void CodeGen::visitType(ASTNS::Type *ast) {}
void CodeGen::visitUnaryexpr(ASTNS::Unaryexpr *ast) {}
void CodeGen::visitVarstmt(ASTNS::Varstmt *ast) {}
void CodeGen::visitVarstmtfinisher(ASTNS::Varstmtfinisher *ast) {}
void CodeGen::visitEmptystmt(ASTNS::Emptystmt *ast) {}
