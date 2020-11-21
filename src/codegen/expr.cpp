#include "codegen/codegen.h"
#include "message/errors.h"

CodeGenNS::ExprCodeGen::ExprCodeGen(CodeGen &cg): cg(cg) {}

Value* CodeGenNS::ExprCodeGen::expr(ASTNS::ExprB *ast)
{
    return nullptr;
}

#define BASICBINARYOP(exprtype) \
    void CodeGenNS::ExprCodeGen::visit##exprtype##Expr(ASTNS::exprtype##Expr *ast) {}

#define BASICUNARYOP(exprtype) \
    void CodeGenNS::ExprCodeGen::visit##exprtype##Expr(ASTNS::exprtype##Expr *ast) {}

BASICBINARYOP(Addition)
BASICBINARYOP(Binand)
BASICBINARYOP(Binor)
BASICBINARYOP(Bitand)
BASICBINARYOP(Bitor)
BASICBINARYOP(Bitshift)
BASICBINARYOP(Bitxor)
BASICBINARYOP(Compeq)
BASICBINARYOP(Complgt)
BASICBINARYOP(Mult)

BASICUNARYOP(Binnot)
BASICUNARYOP(Unary)

void CodeGenNS::ExprCodeGen::visitCallExpr(ASTNS::CallExpr *ast) {}
void CodeGenNS::ExprCodeGen::visitPrimaryExpr(ASTNS::PrimaryExpr *ast) {}
void CodeGenNS::ExprCodeGen::visitTernaryExpr(ASTNS::TernaryExpr *ast) {}
void CodeGenNS::ExprCodeGen::visitAssignmentExpr(ASTNS::AssignmentExpr *ast) {}
