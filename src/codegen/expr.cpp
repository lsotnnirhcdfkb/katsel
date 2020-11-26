#include "codegen/codegen.h"
#include "message/errors.h"

CodeGenNS::ExprCodeGen::ExprCodeGen(CodeGen &cg): cg(cg) {}

IR::ASTValue CodeGenNS::ExprCodeGen::expr(ASTNS::ExprB *ast)
{
    ret = IR::ASTValue();
    ast->accept(this);
    return ret;
}

#define BASICBINARYOP(exprtype)                                                                                                           \
    void CodeGenNS::ExprCodeGen::visit##exprtype##Expr(ASTNS::exprtype##Expr *ast)                                                        \
    {                                                                                                                                     \
        IR::ASTValue lhs = expr(ast->lhs.get());                                                                                          \
        IR::ASTValue rhs = expr(ast->rhs.get());                                                                                          \
                                                                                                                                          \
        if (!lhs || !rhs)                                                                                                                 \
        {                                                                                                                                 \
            ret = IR::ASTValue();                                                                                                         \
            return;                                                                                                                       \
        }                                                                                                                                 \
                                                                                                                                          \
        if (!lhs.type()->hasOperator(ast->op.type))                                                                                       \
        {                                                                                                                                 \
            Error(Error::MsgType::ERROR, ast->op, "left-hand side of binary expression does not support operator")                        \
                .underline(Error::Underline(ast->op, '^')                                                                                 \
                    .error(concatMsg("type \"", lhs.type()->stringify(), "\" does not support operator \"", ast->op.stringify(), "\"")))  \
                .underline(Error::Underline(lhs, '~'))                                                                                    \
                .underline(Error::Underline(rhs, '-'))                                                                                    \
                .report();                                                                                                                \
                ret = IR::ASTValue();                                                                                                     \
            cg.errored = true;                                                                                                            \
            return;                                                                                                                       \
        }                                                                                                                                 \
                                                                                                                                          \
        ret = lhs.type()->binOp(cg.context, lhs, rhs, ast->op, ast);                                                                      \
    }

#define BASICUNARYOP(exprtype)                                                                                                                        \
    void CodeGenNS::ExprCodeGen::visit##exprtype##Expr(ASTNS::exprtype##Expr *ast)                                                                    \
    {                                                                                                                                                 \
        IR::ASTValue oper = expr(ast->operand.get());                                                                                                 \
        if (!oper)                                                                                                                                    \
        {                                                                                                                                             \
            ret = IR::ASTValue();                                                                                                                     \
            return;                                                                                                                                   \
        }                                                                                                                                             \
                                                                                                                                                      \
        if (!oper.type()->hasOperator(ast->op.type))                                                                                                  \
        {                                                                                                                                             \
            Error(Error::MsgType::ERROR, ast->operand.get(), "operand of unary expression does not support operator")                                 \
                .underline(Error::Underline(ast->op, '^')                                                                                             \
                    .error(concatMsg("operand of type \"", oper.type()->stringify(), "\" does not support operator \"", ast->op.stringify(), "\"")))  \
                .underline(Error::Underline(oper, '-'))                                                                                               \
                .report();                                                                                                                            \
            ret = IR::ASTValue();                                                                                                                     \
            cg.errored = true;                                                                                                                        \
            return;                                                                                                                                   \
        }                                                                                                                                             \
                                                                                                                                                      \
        ret = oper.type()->unaryOp(cg.context, oper, ast->op, ast);                                                                                   \
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

BASICUNARYOP(Unary)

void CodeGenNS::ExprCodeGen::visitCallExpr(ASTNS::CallExpr *ast)
{
    IR::ASTValue func = expr(ast->callee.get());
    if (!func)
    {
        ret = IR::ASTValue();
        return;
    }

    IR::FunctionType *fty = dynamic_cast<IR::FunctionType*>(func.type());
    if (!fty)
    {
        Error(Error::MsgType::ERROR, ast->oparn, "value not callable")
            .underline(Error::Underline(func, '^')
                .error(concatMsg("cannot call non-function of type \"", func.type()->stringify(), "\"")))
            .report();
        ret = IR::ASTValue();
        cg.errored = true;
        return;
    }

    IR::Type *retty = fty->ret;
    std::vector<IR::ASTValue> args;
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
        cg.errored = true;
        ret = IR::ASTValue();
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

        if ((*i).type() != *j)
        {
            Error(Error::MsgType::ERROR, *i, "wrong argument to function call")
                .underline(Error::Underline(*i, '^')
                    .error(concatMsg("wrong argument to function call: argument is of type \"", (*i).type()->stringify(), "\""))
                    .note(concatMsg("passing to parameter of type \"", (*j)->stringify(), "\"")))
                .report();
            ret = IR::ASTValue();
            cg.errored = true;
            return;
        }
    }

    if (argserr)
    {
        ret = IR::ASTValue();
        return;
    }

    IR::Register *outReg = nullptr;
    if (!dynamic_cast<IR::VoidType*>(retty))
        outReg = cg.context.curFunc->addRegister(retty, ast);

    cg.context.curBlock->add(std::make_unique<IR::Instrs::Call>(outReg, static_cast<IR::Function *>(func.val), args));

    ret = IR::ASTValue(outReg, ast);
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
            ret = IR::ASTValue(cg.context.getConstInt(cg.context.getBuiltinType(IR::BuiltinType::Builtins::BOOL), 1), ast);
            return;

        case TokenType::FALSELIT:
            ret = IR::ASTValue(cg.context.getConstInt(cg.context.getBuiltinType(IR::BuiltinType::Builtins::BOOL), 0), ast);
            return;

        case TokenType::FLOATLIT:
            Error(Error::MsgType::INTERR, ast->value, "floating point literals are not supported yet")
                .underline(Error::Underline(ast->value, '^')
                        .note("coming soon!"))
                .reportAbort();

        case TokenType::NULLPTRLIT:
            Error(Error::MsgType::INTERR, ast->value, "nullptr literals are not supported yet")
                .underline(Error::Underline(ast->value, '^')
                        .error("pointers are not here yet!")
                        .note("coming soon!"))
                .reportAbort();

        case TokenType::DECINTLIT:
            ret = IR::ASTValue(cg.context.getConstInt(cg.context.getBuiltinType(IR::BuiltinType::Builtins::UINT32), std::stoi(ast->value.stringify())), ast);
            return;

        case TokenType::OCTINTLIT:
            ret = IR::ASTValue(cg.context.getConstInt(cg.context.getBuiltinType(IR::BuiltinType::Builtins::UINT32), std::stoi(ast->value.stringify(), nullptr, 8)), ast);
            return;

        case TokenType::BININTLIT:
            ret = IR::ASTValue(cg.context.getConstInt(cg.context.getBuiltinType(IR::BuiltinType::Builtins::UINT32), std::stoi(ast->value.stringify(), nullptr, 2)), ast);
            return;

        case TokenType::HEXINTLIT:
            ret = IR::ASTValue(cg.context.getConstInt(cg.context.getBuiltinType(IR::BuiltinType::Builtins::UINT32), std::stoi(ast->value.stringify(), nullptr, 16)), ast);
            return;

        case TokenType::CHARLIT:
            ret = IR::ASTValue(cg.context.getConstInt(cg.context.getBuiltinType(IR::BuiltinType::Builtins::CHAR), *(ast->value.start + 1)), ast);
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
                IR::Value *v = cg.context.findValue(ast->value.stringify());
                if (!v)
                {
                    Error(Error::MsgType::ERROR, ast->value, "name is not defined")
                        .underline(Error::Underline(ast->value, '^')
                                .error("name is not defined"))
                        .report();
                    ret = IR::ASTValue();
                    cg.errored = true;
                    return;
                }
                ret = IR::ASTValue(v, ast);
            }
            return;

        default:
            invalidTok("primary token", ast->value);
    }
}
void CodeGenNS::ExprCodeGen::visitTernaryExpr(ASTNS::TernaryExpr *ast)
{
    IR::ASTValue cond = expr(ast->cond.get());
    if (!cond)
    {
        ret = IR::ASTValue();
        return;
    }
    cond = cond.type()->isTrue(cg.context, cond);

    IR::Block *trueb  = cg.context.curFunc->addBlock("ternary_true");
    IR::Block *falseb = cg.context.curFunc->addBlock("ternary_false");
    IR::Block *afterb = cg.context.curFunc->addBlock("ternary_after");

    cg.context.curBlock->branch(std::make_unique<IR::Instrs::CondBr>(cond, trueb, falseb));

    cg.context.curBlock = trueb;
    IR::ASTValue truev = expr(ast->trues.get());
    if (!truev)
    {
        ret = IR::ASTValue();
        return;
    }
    cg.context.curBlock->branch(std::make_unique<IR::Instrs::GotoBr>(afterb));
    trueb = cg.context.curBlock;

    cg.context.curBlock = falseb;
    IR::ASTValue falsev = expr(ast->falses.get());
    if (!falsev)
    {
        ret = IR::ASTValue();
        return;
    }
    cg.context.curBlock->branch(std::make_unique<IR::Instrs::GotoBr>(afterb));
    falseb = cg.context.curBlock;

    if (truev.type() != falsev.type())
    {
        Error(Error::MsgType::ERROR, ast->colon, "conflicting types for ternary expression")
            .underline(Error::Underline(truev, '^')
                .note(truev.type()->stringify()))
            .underline(Error::Underline(falsev, '^')
                .note(falsev.type()->stringify()))
            .underline(Error::Underline(ast->colon, '-'))
            .underline(Error::Underline(ast->quest, '-'))
            .report();
        ret = IR::ASTValue();
        cg.errored = true;
        return;
    }

    cg.context.curBlock = afterb;
    IR::Register *outreg = cg.context.curFunc->addRegister(truev.type(), ast);

    trueb->add(std::make_unique<IR::Instrs::Store>(outreg, truev));
    falseb->add(std::make_unique<IR::Instrs::Store>(outreg, falsev));

    ret = IR::ASTValue(outreg, ast);
}

void CodeGenNS::ExprCodeGen::visitAssignmentExpr(ASTNS::AssignmentExpr *ast)
{
    IR::ASTValue lhs = expr(ast->target.get());
    IR::ASTValue rhs = expr(ast->value.get());

    if (!lhs || !rhs)
    {
        ret = IR::ASTValue();
        return;
    }

    IR::Register *targetReg = dynamic_cast<IR::Register*>(lhs.val);

    if (!lhs.assignable() || !targetReg)
    {
        Error(Error::MsgType::ERROR, ast->equal, "invalid assignment target")
            .underline(Error::Underline(ast->equal, '^')
                .error("invalid assignment target"))
            .underline(Error::Underline(lhs, '~'))
            .report();
        ret = IR::ASTValue();
        cg.errored = true;
        return;
    }

    if (targetReg->type() != rhs.type())
    {
        Error(Error::MsgType::ERROR, ast->equal, "assignment target and value do not have same type")
            .underline(Error::Underline(rhs, '^')
                .note(rhs.type()->stringify()))
            .underline(Error::Underline(lhs, '^')
                .note(targetReg->type()->stringify()))
            .underline(Error::Underline(ast->equal, '-'))
            .report();
        ret = IR::ASTValue();
        cg.errored = true;
        return;
    }

    cg.context.curBlock->add(std::make_unique<IR::Instrs::Store>(targetReg, rhs));

    ret = rhs;
}
