#include "codegen/codegen.h"
#include "message/fmtmessage.h"
#include "message/errors.h"

void CodeGen::visitBinaryExpr(ASTNS::BinaryExpr *a)
{
    Value lhs = evalExpr(a->lhs.get());
    Value rhs = evalExpr(a->rhs.get());

    if (!lhs.type->hasOperator(a->op.type))
    {
        reportError(a->op, msg::typeNoOp(lhs.type, a->op));
        exprRetVal = Value();
        return;
    }

    exprRetVal = lhs.type->binOp(context, lhs, rhs, a->op);
}

void CodeGen::visitUnaryExpr(ASTNS::UnaryExpr *a)
{
	Value oper = evalExpr(a->operand.get());

    if (!oper.type->hasOperator(a->op.type))
    {
        reportError(a->op, msg::typeNoOp(oper.type, a->op));
        exprRetVal = Value();
        return;
    }

    exprRetVal = oper.type->unaryOp(context, oper, a->op);
}

void CodeGen::visitTernaryExpr(ASTNS::TernaryExpr *a)
{
	
}

void CodeGen::visitPrimaryExpr(ASTNS::PrimaryExpr *a)
{
	
}

void CodeGen::visitCallExpr(ASTNS::CallExpr *a)
{
	
}

