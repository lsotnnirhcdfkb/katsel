#include "codegen/codegen.h"
#include "message/errors.h"

CodeGenNS::ExprCodeGen::ExprCodeGen(CodeGen &cg): cg(cg) {}

Value CodeGenNS::ExprCodeGen::expr(ASTNS::ExprB *ast)
{
    ret = Value();
    ast->accept(this);
    return ret;
}

void CodeGenNS::ExprCodeGen::visitAdditionExpr(ASTNS::AdditionExpr *ast)
{

}
void CodeGenNS::ExprCodeGen::visitAssignmentExpr(ASTNS::AssignmentExpr *ast)
{

}
void CodeGenNS::ExprCodeGen::visitBinandExpr(ASTNS::BinandExpr *ast)
{

}
void CodeGenNS::ExprCodeGen::visitBinnotExpr(ASTNS::BinnotExpr *ast)
{

}
void CodeGenNS::ExprCodeGen::visitBinorExpr(ASTNS::BinorExpr *ast)
{

}
void CodeGenNS::ExprCodeGen::visitBitandExpr(ASTNS::BitandExpr *ast)
{

}
void CodeGenNS::ExprCodeGen::visitBitorExpr(ASTNS::BitorExpr *ast)
{

}
void CodeGenNS::ExprCodeGen::visitBitshiftExpr(ASTNS::BitshiftExpr *ast)
{

}
void CodeGenNS::ExprCodeGen::visitBitxorExpr(ASTNS::BitxorExpr *ast)
{

}
void CodeGenNS::ExprCodeGen::visitCallExpr(ASTNS::CallExpr *ast)
{

}
void CodeGenNS::ExprCodeGen::visitCompeqExpr(ASTNS::CompeqExpr *ast)
{

}
void CodeGenNS::ExprCodeGen::visitComplgtExpr(ASTNS::ComplgtExpr *ast)
{

}
void CodeGenNS::ExprCodeGen::visitExpr(ASTNS::Expr *ast)
{

}
void CodeGenNS::ExprCodeGen::visitMultExpr(ASTNS::MultExpr *ast)
{

}
void CodeGenNS::ExprCodeGen::visitPrimaryExpr(ASTNS::PrimaryExpr *ast)
{

}
void CodeGenNS::ExprCodeGen::visitTernaryExpr(ASTNS::TernaryExpr *ast)
{

}
void CodeGenNS::ExprCodeGen::visitUnaryExpr(ASTNS::UnaryExpr *ast)
{

}

