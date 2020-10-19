#include "codegen/codegen.h"
#include "message/fmtmessage.h"
#include "message/errors.h"

void CodeGen::visitBinaryExpr(ASTNS::BinaryExpr *a)
{
    // TODO: assignment
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
	Value cond = evalExpr(a->condition.get());
    cond = cond.type->isTrue(context, cond);
    Value truev = evalExpr(a->trues.get());
    Value falsev = evalExpr(a->falses.get());

    if (truev.type != falsev.type)
    {
        // TODO
        std::cerr << "ternary expression operands of different types are currently not supported" << std::endl;
        std::abort();
    }

    exprRetVal = Value(truev.type, context.builder.CreateSelect(cond.val, truev.val, falsev.val));
}

void CodeGen::visitPrimaryExpr(ASTNS::PrimaryExpr *a)
{
	
}

void CodeGen::visitCallExpr(ASTNS::CallExpr *a)
{
	
}

