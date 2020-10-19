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
    Value ret;
    switch (a->value.type)
    {
        case TokenType::TRUELIT:
            ret = Value(context.getBuiltinType(BuiltinType::Builtins::BOOL), context.builder.getInt1(true));
            break;

        case TokenType::FALSELIT:
            ret = Value(context.getBuiltinType(BuiltinType::Builtins::BOOL), context.builder.getInt1(false));
            break;

        case TokenType::FLOATLIT:
            break;

        case TokenType::NULLPTRLIT:
            std::cerr << "nullptr literal is not supported because we do not have pointers yet ahhaahahhaaa" << std::endl;
            std::abort();
            break;

        case TokenType::DECINTLIT:
            ret = Value(context.getBuiltinType(BuiltinType::Builtins::UINT32), context.builder.getInt32(std::stoi(tokenToStr(a->value))));
            break;

        case TokenType::OCTINTLIT:
            ret = Value(context.getBuiltinType(BuiltinType::Builtins::UINT32), context.builder.getInt32(std::stoi(tokenToStr(a->value), nullptr, 8)));
            break;

        case TokenType::BININTLIT:
            ret = Value(context.getBuiltinType(BuiltinType::Builtins::UINT32), context.builder.getInt32(std::stoi(tokenToStr(a->value), nullptr, 2)));
            break;

        case TokenType::HEXINTLIT:
            ret = Value(context.getBuiltinType(BuiltinType::Builtins::UINT32), context.builder.getInt32(std::stoi(tokenToStr(a->value), nullptr, 16)));
            break;

        case TokenType::CHARLIT:
            ret = Value(context.getBuiltinType(BuiltinType::Builtins::CHAR), context.builder.getInt8(*(a->value.start + 1)));
            break;

        case TokenType::STRINGLIT:
            std::cerr << "string literals are not supported yet" << std::endl;
            break;

        case TokenType::IDENTIFIER:
            {
                Value v = context.findValue(tokenToStr(a->value));
                if (!v.val)
                {
                    reportError(a->value, msg::undefVar());
                    return;
                }
                llvm::Value *loadInst = context.builder.CreateLoad(v.val);
                ret = Value(v.type, loadInst);
            }
            break;

        default:
            break; // Unreachable
    }
    exprRetVal = ret;
}

void CodeGen::visitCallExpr(ASTNS::CallExpr *a)
{
	
}

