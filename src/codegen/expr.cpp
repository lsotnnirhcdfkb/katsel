#include "codegen/codegen.h"
#include "message/errors.h"

CodeGenNS::ExprCodeGen::ExprCodeGen(CodeGen &cg): cg(cg) {}

Value* CodeGenNS::ExprCodeGen::expr(ASTNS::ExprB *ast)
{
    ret = nullptr;
    ast->accept(this);
    return ret;
}

#define BASICBINARYOP(exprtype)                                                                                                           \
    void CodeGenNS::ExprCodeGen::visit##exprtype##Expr(ASTNS::exprtype##Expr *ast)                                                        \
    {                                                                                                                                     \
        Value *lhs = expr(ast->lhs.get());                                                                                                \
        Value *rhs = expr(ast->rhs.get());                                                                                                \
                                                                                                                                          \
        if (!lhs || !rhs)                                                                                                                 \
        {                                                                                                                                 \
            ret = nullptr;                                                                                                                \
            return;                                                                                                                       \
        }                                                                                                                                 \
                                                                                                                                          \
        if (!lhs->type()->hasOperator(ast->op.type))                                                                                      \
        {                                                                                                                                 \
            Error(Error::MsgType::ERROR, ast->op, "left-hand side of binary expression does not support operator")                        \
                .underline(Error::Underline(ast->op, '^')                                                                                 \
                    .error(concatMsg("type \"", lhs->type()->stringify(), "\" does not support operator \"", ast->op.stringify(), "\""))) \
                .underline(Error::Underline(lhs, '~'))                                                                                    \
                .underline(Error::Underline(rhs, '-'))                                                                                    \
                .report();                                                                                                                \
                ret = nullptr;                                                                                                            \
                return;                                                                                                                   \
        }                                                                                                                                 \
                                                                                                                                          \
        ret = lhs->type()->binOp(cg.context, lhs, rhs, ast->op, ast);                                                                     \
    }

#define BASICUNARYOP(exprtype)                                                                                                                        \
    void CodeGenNS::ExprCodeGen::visit##exprtype##Expr(ASTNS::exprtype##Expr *ast)                                                                    \
    {                                                                                                                                                 \
        Value *oper = expr(ast->operand.get());                                                                                                       \
        if (!oper)                                                                                                                                    \
        {                                                                                                                                             \
            ret = nullptr;                                                                                                                            \
            return;                                                                                                                                   \
        }                                                                                                                                             \
                                                                                                                                                      \
        if (!oper->type()->hasOperator(ast->op.type))                                                                                                 \
        {                                                                                                                                             \
            Error(Error::MsgType::ERROR, ast->operand.get(), "operand of unary expression does not support operator")                                 \
                .underline(Error::Underline(ast->op, '^')                                                                                             \
                    .error(concatMsg("operand of type \"", oper->type()->stringify(), "\" does not support operator \"", ast->op.stringify(), "\""))) \
                .underline(Error::Underline(oper, '-'))                                                                                               \
                .report();                                                                                                                            \
            ret = nullptr;                                                                                                                            \
            return;                                                                                                                                   \
        }                                                                                                                                             \
                                                                                                                                                      \
        ret = oper->type()->unaryOp(cg.context, oper, ast->op, ast);                                                                                  \
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
    Value *func = expr(ast->callee.get());
    if (!func)
    {
        ret = nullptr;
        return;
    }

    FunctionType *fty = dynamic_cast<FunctionType*>(func->type());
    if (!fty)
    {
        Error(Error::MsgType::ERROR, ast->oparn, "value not callable")
            .underline(Error::Underline(func, '^')
                .error(concatMsg("cannot call non-function of type \"", func->type()->stringify(), "\"")))
            .report();
        ret = nullptr;
        return;
    }

    Type *retty = fty->ret;
    std::vector<Value*> args;
    if (ast->args)
        args = cg.argsVisitor.args(ast->args.get());

    if (args.size() != fty->paramtys.size())
    {
        Error(Error::MsgType::ERROR, ast->oparn, "wrong number of arguments to function call")
            .underline(Error::Underline(ast->args.get(), '^')
                .error("wrong number of arguments to function call"))
            .underline(Error::Underline(func, '-')
                .note(concatMsg("function expects ", fty->paramtys.size(), " arguments, but got ", args.size(), " arguments")))
            .report();
        ret = nullptr;
        return;
    }

    bool argserr = false;
    auto i = args.begin();
    auto j = fty->paramtys.begin();
    for (; i != args.end() && j != fty->paramtys.end(); ++i, ++j)
    {
        if (!*i)
        {
            argserr = true;
            continue;
        }

        if ((*i)->type() != *j)
        {
            Error(Error::MsgType::ERROR, *i, "wrong argument to function call")
                .underline(Error::Underline(*i, '^')
                    .error(concatMsg("wrong argument to function call: argument is of type \"", (*i)->type()->stringify(), "\""))
                    .note(concatMsg("passing to parameter of type \"", (*j)->stringify(), "\"")))
                .report();
            ret = nullptr;
            return;
        }
    }

    if (argserr)
    {
        ret = nullptr;
        return;
    }

    Register *outReg = nullptr;
    if (!dynamic_cast<VoidType*>(retty))
        outReg = cg.context.curFunc->addRegister(retty, ast);

    cg.context.curBlock->add(std::make_unique<Instrs::Call>(outReg, static_cast<Function*>(func), args));

    ret = outReg;
}

void CodeGenNS::ExprCodeGen::visitPrimaryExpr(ASTNS::PrimaryExpr *ast)
{
    if (ast->form == ASTNS::PrimaryExpr::Form::TAT)
    {
        ast->expr->accept(this);
        return;
    }

    switch (ast->value.type)
    {
        case TokenType::TRUELIT:
            ret = cg.context.getConstInt(cg.context.getBuiltinType(BuiltinType::Builtins::BOOL), 1, ast);
            return;

        case TokenType::FALSELIT:
            ret = cg.context.getConstInt(cg.context.getBuiltinType(BuiltinType::Builtins::BOOL), 0, ast);
            return;

        case TokenType::FLOATLIT:
            Error(Error::MsgType::INTERR, ast->value, "floating point literals are not supported yet")
                .underline(Error::Underline(ast->value, '^')
                        .note("coming soon!"))
                .reportAbort();
            return;

        case TokenType::NULLPTRLIT:
            Error(Error::MsgType::INTERR, ast->value, "nullptr literals are not supported yet")
                .underline(Error::Underline(ast->value, '^')
                        .error("pointers are not here yet!")
                        .note("coming soon!"))
                .reportAbort();

        case TokenType::DECINTLIT:
            ret = cg.context.getConstInt(cg.context.getBuiltinType(BuiltinType::Builtins::UINT32), std::stoi(ast->value.stringify()), ast);
            return;

        case TokenType::OCTINTLIT:
            ret = cg.context.getConstInt(cg.context.getBuiltinType(BuiltinType::Builtins::UINT32), std::stoi(ast->value.stringify(), nullptr, 8), ast);
            return;

        case TokenType::BININTLIT:
            ret = cg.context.getConstInt(cg.context.getBuiltinType(BuiltinType::Builtins::UINT32), std::stoi(ast->value.stringify(), nullptr, 2), ast);
            return;

        case TokenType::HEXINTLIT:
            ret = cg.context.getConstInt(cg.context.getBuiltinType(BuiltinType::Builtins::UINT32), std::stoi(ast->value.stringify(), nullptr, 16), ast);
            return;

        case TokenType::CHARLIT:
            ret = cg.context.getConstInt(cg.context.getBuiltinType(BuiltinType::Builtins::CHAR), *(ast->value.start + 1), ast);
            return;

        case TokenType::STRINGLIT:
            Error(Error::MsgType::INTERR, ast->value, "string literals are not supported yet")
                .underline(Error::Underline(ast->value, '^')
                        .error("strings")
                        .note("coming soon!")
                        .note("probably after nullptr literals though!"))
                .reportAbort();

        case TokenType::IDENTIFIER:
            {
                Value *v = cg.context.findValue(ast->value.stringify());
                if (!v)
                {
                    Error(Error::MsgType::ERROR, ast->value, "name is not defined")
                        .underline(Error::Underline(ast->value, '^')
                                .error("name is not defined"))
                        .report();
                    ret = nullptr;
                    return;
                }
                ret = v; // TODO: somehow associate a different ast with this value
                         // TODO: by creating a register with a reference type
            }
            return;

        default:
            invalidTok("primary token", ast->value);
    }
}
void CodeGenNS::ExprCodeGen::visitTernaryExpr(ASTNS::TernaryExpr *ast)
{
	Value *cond = expr(ast->cond.get());
    if (!cond)
    {
        ret = nullptr;
        return;
    }
    cond = cond->type()->isTrue(cg.context, cond);

    Block *trueb  = cg.context.curFunc->addBlock("ternary_true");
    Block *falseb = cg.context.curFunc->addBlock("ternary_false");
    Block *afterb = cg.context.curFunc->addBlock("ternary_after");

    cg.context.curBlock->branch(std::make_unique<Instrs::CondBr>(cond, trueb, falseb));

    cg.context.curBlock = trueb;
    Value *truev = expr(ast->trues.get());
    if (!truev)
    {
        ret = nullptr;
        return;
    }
    cg.context.curBlock->branch(std::make_unique<Instrs::GotoBr>(afterb));
    trueb = cg.context.curBlock;

    cg.context.curBlock = falseb;
    Value *falsev = expr(ast->falses.get());
    if (!falsev)
    {
        ret = nullptr;
        return;
    }
    cg.context.curBlock->branch(std::make_unique<Instrs::GotoBr>(afterb));
    falseb = cg.context.curBlock;

    if (truev->type() != falsev->type())
    {
        Error(Error::MsgType::ERROR, ast->colon, "conflicting types for ternary expression")
            .underline(Error::Underline(truev, '^')
                .note(truev->type()->stringify()))
            .underline(Error::Underline(falsev, '^')
                .note(falsev->type()->stringify()))
            .underline(Error::Underline(ast->colon, '-'))
            .underline(Error::Underline(ast->quest, '-'))
            .report();
        ret = nullptr;
        return;
    }

    cg.context.curBlock = afterb;
    Register *outreg = cg.context.curFunc->addRegister(truev->type(), ast);

    trueb->add(std::make_unique<Instrs::Store>(outreg, truev));
    falseb->add(std::make_unique<Instrs::Store>(outreg, falsev));

    ret = outreg;
}

void CodeGenNS::ExprCodeGen::visitAssignmentExpr(ASTNS::AssignmentExpr *ast)
{
    Value *lhs = expr(ast->target.get());
    Value *rhs = expr(ast->value.get());

    if (!lhs || !rhs)
    {
        ret = nullptr;
        return;
    }

    Register *targetReg = dynamic_cast<Register*>(lhs);

    if (!lhs->assignable() || !targetReg)
    {
        Error(Error::MsgType::ERROR, ast->equal, "invalid assignment target")
            .underline(Error::Underline(ast->equal, '^')
                .error("invalid assignment target"))
            .underline(Error::Underline(lhs, '~'))
            .report();
        ret = nullptr;
        return;
    }

    if (targetReg->type() != rhs->type())
    {
        Error(Error::MsgType::ERROR, ast->equal, "assignment target and value do not have same type")
            .underline(Error::Underline(rhs, '^')
                .note(rhs->type()->stringify()))
            .underline(Error::Underline(targetReg, '^')
                .note(targetReg->type()->stringify()))
            .underline(Error::Underline(ast->equal, '-'))
            .report();
        ret = nullptr;
        return;
    }

    cg.context.curBlock->add(std::make_unique<Instrs::Store>(targetReg, rhs));

    ret = rhs;
}
