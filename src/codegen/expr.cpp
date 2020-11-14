#include "codegen/codegen.h"
#include "message/errors.h"

CodeGenNS::ExprCodeGen::ExprCodeGen(CodeGen &cg): cg(cg) {}

Value CodeGenNS::ExprCodeGen::expr(ASTNS::ExprB *ast)
{
    ret = Value();
    ast->accept(this);
    return ret;
}

#define BASICBINARYOP(exprtype)                                                                                                        \
    void CodeGenNS::ExprCodeGen::visit##exprtype##Expr(ASTNS::exprtype##Expr *ast)                                                     \
    {                                                                                                                                  \
        Value lhs = expr(ast->lhs.get());                                                                                              \
        Value rhs = expr(ast->rhs.get());                                                                                              \
                                                                                                                                       \
        if (!lhs.val || !rhs.val)                                                                                                      \
        {                                                                                                                              \
            ret = Value();                                                                                                             \
            return;                                                                                                                    \
        }                                                                                                                              \
                                                                                                                                       \
        if (!lhs.type->hasOperator(ast->op.type))                                                                                      \
        {                                                                                                                              \
            Error(Error::MsgType::ERROR, ast->op, "left-hand side of binary expression does not support operator")                     \
                .underline(Error::Underline(ast->op, '^')                                                                              \
                    .error(concatMsg("type "", lhs.type->stringify(), "" does not support operator "", ast->op.stringify(), """)))     \
                .underline(Error::Underline(lhs, '~'))                                                                                 \
                .underline(Error::Underline(rhs, '-'))                                                                                 \
                .report();                                                                                                             \
            ret = Value();                                                                                                             \
            return;                                                                                                                    \
        }                                                                                                                              \
                                                                                                                                       \
        ret = lhs.type->binOp(cg.context, lhs, rhs, ast->op, ast);                                                                     \
    }

#define BASICUNARYOP(exprtype)                                                                                                                        \
    void CodeGenNS::ExprCodeGen::visit##exprtype##Expr(ASTNS::exprtype##Expr *ast)                                                                    \
    {                                                                                                                                                 \
        Value oper = expr(ast->operand.get());                                                                                                        \
        if (!oper.val)                                                                                                                                \
        {                                                                                                                                             \
            ret = Value();                                                                                                                            \
            return;                                                                                                                                   \
        }                                                                                                                                             \
                                                                                                                                                      \
        if (!oper.type->hasOperator(ast->op.type))                                                                                                    \
        {                                                                                                                                             \
            Error(Error::MsgType::ERROR, ast->operand.get(), "operand of unary expression does not support operator")                                 \
                .underline(Error::Underline(ast->op, '^')                                                                                             \
                    .error(concatMsg("operand of type \"", oper.type->stringify(), "\" does not support operator \"", ast->op.stringify(), "\"")))    \
                .underline(Error::Underline(oper, '-'))                                                                                               \
                .report();                                                                                                                            \
                ret = Value();                                                                                                                        \
                return;                                                                                                                               \
        }                                                                                                                                             \
                                                                                                                                                      \
        ret = oper.type->unaryOp(cg.context, oper, ast->op, ast);                                                                                     \
    }

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

void CodeGenNS::ExprCodeGen::visitCallExpr(ASTNS::CallExpr *ast)
{

}
void CodeGenNS::ExprCodeGen::visitPrimaryExpr(ASTNS::PrimaryExpr *ast)
{

}
void CodeGenNS::ExprCodeGen::visitTernaryExpr(ASTNS::TernaryExpr *ast)
{

}
void CodeGenNS::ExprCodeGen::visitAssignmentExpr(ASTNS::AssignmentExpr *ast)
{

}

void CodeGenNS::ExprCodeGen::visitExpr(ASTNS::Expr *) {}
