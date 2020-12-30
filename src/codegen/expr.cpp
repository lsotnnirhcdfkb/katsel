#include "codegenlocal.h"
#include "message/internal.h"
#include "message/reportAbort.h"
#include "message/errmsgs.h"

CodeGen::FunctionCodeGen::ExprCodeGen::ExprCodeGen(CodeGen &cg, FunctionCodeGen &fcg): cg(cg), fcg(fcg) {}

IR::ASTValue CodeGen::FunctionCodeGen::ExprCodeGen::expr(ASTNS::Expr *ast)
{
    ret = IR::ASTValue();
    ast->accept(this);
    return ret;
}

void CodeGen::FunctionCodeGen::ExprCodeGen::visitBinaryExpr(ASTNS::BinaryExpr *ast)
{
    IR::ASTValue lhs = expr(ast->lhs.get()); \
    IR::ASTValue rhs = expr(ast->rhs.get()); \
    if (!lhs || !rhs)                        \
    {                                        \
        ret = IR::ASTValue();                \
        return;                              \
    }
    IR::Type::BinaryOperator oper;
    switch (ast->op.type)
    {
        case TokenType::BANGEQUAL: oper = IR::Type::BinaryOperator::bangequal; break;
        case TokenType::DOUBLEEQUAL: oper = IR::Type::BinaryOperator::doubleequal; break;
        case TokenType::LESS: oper = IR::Type::BinaryOperator::less; break;
        case TokenType::GREATER: oper = IR::Type::BinaryOperator::greater; break;
        case TokenType::LESSEQUAL: oper = IR::Type::BinaryOperator::lessequal; break;
        case TokenType::GREATEREQUAL: oper = IR::Type::BinaryOperator::greaterequal; break;
        case TokenType::CARET: oper = IR::Type::BinaryOperator::caret; break;
        case TokenType::PIPE: oper = IR::Type::BinaryOperator::pipe; break;
        case TokenType::AMPER: oper = IR::Type::BinaryOperator::amper; break;
        case TokenType::DOUBLEGREATER: oper = IR::Type::BinaryOperator::doublegreater; break;
        case TokenType::DOUBLELESS: oper = IR::Type::BinaryOperator::doubleless; break;
        case TokenType::PLUS: oper = IR::Type::BinaryOperator::plus; break;
        case TokenType::MINUS: oper = IR::Type::BinaryOperator::minus; break;
        case TokenType::STAR: oper = IR::Type::BinaryOperator::star; break;
        case TokenType::SLASH: oper = IR::Type::BinaryOperator::slash; break;
        case TokenType::PERCENT: oper = IR::Type::BinaryOperator::percent; break;
        default: invalidTok("binary operator", ast->op);
    }
    ret = lhs.type()->binOp(*cg.context, *fcg.fun, fcg.curBlock, oper, lhs, rhs, ast->op, ast);
    if (!ret)
        fcg.errored = true;
}
void CodeGen::FunctionCodeGen::ExprCodeGen::visitShortCircuitExpr(ASTNS::ShortCircuitExpr *ast)
{
    IR::ASTValue lhs = expr(ast->lhs.get());
    if (!lhs)
    {
        ret = IR::ASTValue();
        return;
    }

    if (!dynamic_cast<IR::BoolType*>(lhs.type()))
    {
        ERR_LHS_UNSUPPORTED_OP(lhs, ast->op);
        ret = IR::ASTValue();
        fcg.errored = true;
        return;
    }

    IR::Block *skip = fcg.fun->addBlock("shortcircuit_skip");
    IR::Block *checkboth = fcg.fun->addBlock("shortcircuit_checkboth");
    IR::Block *after = fcg.fun->addBlock("shortcircuit_after");

    bool valueIfSkipped;
    if (ast->op.type == TokenType::DOUBLEPIPE)
    {
        // jump to skip when true
        fcg.curBlock->branch(std::make_unique<IR::Instrs::CondBr>(lhs, skip, checkboth));
        valueIfSkipped = true;
    }
    else if (ast->op.type == TokenType::DOUBLEAMPER)
    {
        // jump to skip when false
        fcg.curBlock->branch(std::make_unique<IR::Instrs::CondBr>(lhs, checkboth, skip));
        valueIfSkipped = false;
    }
    else
        invalidTok("short circuiting operator", ast->op);

    fcg.curBlock = checkboth;
    IR::ASTValue rhs = expr(ast->rhs.get());
    if (!rhs)
    {
        ret = IR::ASTValue();
        return;
    }
    if (!dynamic_cast<IR::BoolType*>(rhs.type()))
    {
        ERR_CONFLICT_TYS_BINARY_OP(lhs, rhs, ast->op);
        ret = IR::ASTValue();
        fcg.errored = true;
        return;
    }
    fcg.curBlock->branch(std::make_unique<IR::Instrs::GotoBr>(after));

    fcg.curBlock = skip;
    fcg.curBlock->branch(std::make_unique<IR::Instrs::GotoBr>(after));

    fcg.curBlock = after;
    ret = IR::ASTValue(fcg.curBlock->add(std::make_unique<IR::Instrs::Phi>(std::vector {std::make_pair(checkboth, rhs), std::make_pair(skip, IR::ASTValue(cg.context->getConstBool(valueIfSkipped), ast))})), ast);
}

void CodeGen::FunctionCodeGen::ExprCodeGen::visitUnaryExpr(ASTNS::UnaryExpr *ast)
{
    IR::ASTValue oper = expr(ast->expr.get());
    if (!oper)
    {
        ret = IR::ASTValue();
        return;
    }

    IR::Type::UnaryOperator opor;

    switch (ast->op.type)
    {
        case TokenType::TILDE:
            opor = IR::Type::UnaryOperator::tilde; break;
        case TokenType::MINUS:
            opor = IR::Type::UnaryOperator::minus; break;
        case TokenType::BANG:
            opor = IR::Type::UnaryOperator::bang; break;
        default:
            invalidTok("unary operator", ast->op);
    }

    ret = oper.type()->unaryOp(*cg.context, *fcg.fun, fcg.curBlock, opor, oper, ast->op, ast);
    if (!ret)
        fcg.errored = true;
}

void CodeGen::FunctionCodeGen::ExprCodeGen::visitDerefExpr(ASTNS::DerefExpr *ast)
{
    IR::ASTValue oper = expr(ast->expr.get());
    if (!oper)
    {
        ret = IR::ASTValue();
        return;
    }

    IR::PointerType *asptrty (dynamic_cast<IR::PointerType*>(oper.type()));
    if (!asptrty)
    {
        ERR_NO_DEREF(ast->op, oper);
        ret = IR::ASTValue();
        fcg.errored = true;
        return;
    }

    ret = IR::ASTValue(fcg.curBlock->add(std::make_unique<IR::Instrs::DerefPtr>(oper)), ast);
}
void CodeGen::FunctionCodeGen::ExprCodeGen::visitAddrofExpr(ASTNS::AddrofExpr *ast)
{
    IR::ASTValue oper = expr(ast->expr.get());
    if (!oper)
    {
        ret = IR::ASTValue();
        return;
    }

    IR::Instrs::DerefPtr *asDeref = dynamic_cast<IR::Instrs::DerefPtr*>(oper.val);
    if (!asDeref)
    {
        ERR_ADDROF_NOT_LVALUE(ast->op, oper);
        ret = IR::ASTValue();
        fcg.errored = true;
        return;
    }

    ret = IR::ASTValue(asDeref->ptr.val, ast);
}

void CodeGen::FunctionCodeGen::ExprCodeGen::visitCallExpr(ASTNS::CallExpr *ast)
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
        ERR_CALL_NONCALLABLE(func, ast->oparn);
        ret = IR::ASTValue();
        fcg.errored = true;
        return;
    }

    std::vector<IR::ASTValue> args;
    if (ast->args)
    {
        CodeGen::ArgVisitor av (fcg);
        ast->args->accept(&av);
        args = av.ret;
    }

    if (args.size() != fty->paramtys.size())
    {
        ERR_WRONG_NUM_ARGS(func, ast->oparn, ast->args.get(), args);
        ret = IR::ASTValue();
        fcg.errored = true;
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

        *i = (*j)->implCast(*cg.context, *fcg.fun, fcg.curBlock, *i);
        if (i->type() != *j)
        {
            ERR_INCORRECT_ARG(*i, *j);
            ret = IR::ASTValue();
            fcg.errored = true;
            argserr = true;
        }
    }

    if (argserr)
    {
        ret = IR::ASTValue();
        return;
    }

    ret = IR::ASTValue(fcg.curBlock->add(std::make_unique<IR::Instrs::Call>(static_cast<IR::Function *>(func.val), args)), ast);
}

void CodeGen::FunctionCodeGen::ExprCodeGen::visitPrimaryExpr(ASTNS::PrimaryExpr *ast)
{
    int _intbase;
    switch (ast->value.type)
    {
        case TokenType::TRUELIT:
            ret = IR::ASTValue(cg.context->getConstBool(true), ast);
            return;

        case TokenType::FALSELIT:
            ret = IR::ASTValue(cg.context->getConstBool(false), ast);
            return;

        case TokenType::FLOATLIT:
            ret = IR::ASTValue(cg.context->getConstFloat(cg.context->getGenericFloatType(), std::stod(ast->value.stringify())), ast);
            return;

        case TokenType::NULLPTRLIT:
            reportAbortNoh("nullptr literals are not supported yet");

        case TokenType::DECINTLIT:
            ret = IR::ASTValue(cg.context->getConstInt(cg.context->getGenericIntType(), std::stoll(ast->value.stringify())), ast);
            return;

        case TokenType::OCTINTLIT:
            _intbase = 8;
            goto makeIntLit;
        case TokenType::BININTLIT:
            _intbase = 2;
            goto makeIntLit;
        case TokenType::HEXINTLIT:
            _intbase = 16;
            goto makeIntLit;

makeIntLit:
            ret = IR::ASTValue(cg.context->getConstInt(cg.context->getGenericIntType(), std::stoll(ast->value.stringify().erase(0, 2), nullptr, _intbase)), ast);
            return;

        case TokenType::CHARLIT:
            ret = IR::ASTValue(cg.context->getConstChar(*(ast->value.start + 1)), ast);
            return;

        case TokenType::STRINGLIT:
            reportAbortNoh("string literals are not supported yet");

        case TokenType::IDENTIFIER:
            {
                std::string name (ast->value.stringify());
                IR::Value *v;

                FunctionCodeGen::Local *l = fcg.getLocal(name);
                if (!l)
                    v = cg.context->getGlobal(name);
                else
                    v = fcg.curBlock->add(std::make_unique<IR::Instrs::DerefPtr>(IR::ASTValue(l->v, ast)));

                if (!v)
                {
                    ERR_UNDECL_SYMB(ast->value);
                    ret = IR::ASTValue();
                    fcg.errored = true;
                    return;
                }
                ret = IR::ASTValue(v, ast);
            }
            return;

        default:
            invalidTok("primary token", ast->value);
    }
}
void CodeGen::FunctionCodeGen::ExprCodeGen::visitIfExpr(ASTNS::IfExpr *ast)
{
    IR::ASTValue cond = expr(ast->cond.get());
    if (!cond)
    {
        ret = IR::ASTValue();
        return;
    }
    if (!dynamic_cast<IR::BoolType*>(cond.type()))
    {
        ERR_COND_NOT_BOOL(cond);
        ret = IR::ASTValue();
        fcg.errored = true;
        return;
    }

    IR::Block *trueb  = fcg.fun->addBlock("if_true");
    IR::Block *falseb = ast->falses ? fcg.fun->addBlock("if_false") : nullptr;
    IR::Block *afterb = fcg.fun->addBlock("if_after");

    if (falseb)
        fcg.curBlock->branch(std::make_unique<IR::Instrs::CondBr>(cond, trueb, falseb));
    else
        fcg.curBlock->branch(std::make_unique<IR::Instrs::CondBr>(cond, trueb, afterb));

    fcg.curBlock = trueb;
    IR::ASTValue truev = expr(ast->trues.get());
    if (!truev)
    {
        ret = IR::ASTValue();
        return;
    }
    fcg.curBlock->branch(std::make_unique<IR::Instrs::GotoBr>(afterb));
    trueb = fcg.curBlock;

    IR::ASTValue falsev;
    if (falseb)
    {
        fcg.curBlock = falseb;
        falsev = expr(ast->falses.get());
        if (!falsev)
        {
            ret = IR::ASTValue();
            return;
        }
        fcg.curBlock->branch(std::make_unique<IR::Instrs::GotoBr>(afterb));
        falseb = fcg.curBlock;
    }

    if (falseb)
    {
        // try both implicit casts -- they are mostly asymmetrical so it should be fine
        truev = falsev.type()->implCast(*cg.context, *fcg.fun, fcg.curBlock, truev);
        falsev = truev.type()->implCast(*cg.context, *fcg.fun, fcg.curBlock, falsev);
        if (truev.type() != falsev.type())
        {
            ERR_CONFL_TYS_IFEXPR(truev, falsev, ast->iftok);
            ret = IR::ASTValue();
            fcg.errored = true;
            return;
        }
    }
    else
    {
        if (!dynamic_cast<IR::VoidType*>(truev.type()))
        {
            ERR_NO_ELSE_NOT_VOID(truev, ast->iftok);
            ret = IR::ASTValue();
            fcg.errored = true;
            return;
        }
    }

    fcg.curBlock = afterb;

    if (falseb)
    {
        ret = IR::ASTValue(afterb->add(std::make_unique<IR::Instrs::Phi>(std::vector {std::make_pair(trueb, truev), std::make_pair(falseb, falsev)})), ast);
    }
    else
        ret = IR::ASTValue(truev.val, ast);

}
void CodeGen::FunctionCodeGen::ExprCodeGen::visitForExpr(ASTNS::ForExpr *ast)
{
    fcg.incScope();

    fcg.stmtCG.stmt(ast->initial.get());

    IR::Block *loopCheckCond = fcg.fun->addBlock("loop_checkcond");
    IR::Block *loopBody = fcg.fun->addBlock("loop_body");
    IR::Block *loopAfter = fcg.fun->addBlock("loop_after");

    fcg.curBlock->branch(std::make_unique<IR::Instrs::GotoBr>(loopCheckCond));
    fcg.curBlock = loopCheckCond;
    IR::ASTValue cond = expr(ast->cond.get());
    if (!cond)
    {
        ret = IR::ASTValue();
        return;
    }
    loopCheckCond->branch(std::make_unique<IR::Instrs::CondBr>(cond, loopBody, loopAfter));

    fcg.curBlock = loopBody;
    expr(ast->body.get());

    expr(ast->increment.get());
    loopBody->branch(std::make_unique<IR::Instrs::GotoBr>(loopCheckCond));
    fcg.decScope();

    fcg.curBlock = loopAfter;

    ret = IR::ASTValue(cg.context->getVoid(), ast);
}

void CodeGen::FunctionCodeGen::ExprCodeGen::visitAssignmentExpr(ASTNS::AssignmentExpr *ast)
{
    IR::ASTValue lhs = expr(ast->target.get());
    IR::ASTValue rhs = expr(ast->expr.get());

    if (!lhs || !rhs)
    {
        ret = IR::ASTValue();
        return;
    }

    IR::Instrs::DerefPtr *targetDeref = dynamic_cast<IR::Instrs::DerefPtr*>(lhs.val);

    if (!targetDeref)
    {
        ERR_ASSIGN_INVALID_LHS(ast->equal, lhs);
        ret = IR::ASTValue();
        fcg.errored = true;
        return;
    }

    IR::Type *expectType = targetDeref->type();
    rhs = expectType->implCast(*cg.context, *fcg.fun, fcg.curBlock, rhs);
    if (expectType != rhs.type())
    {
        ERR_ASSIGN_CONFLICT_TYS(lhs, rhs, ast->equal);
        ret = IR::ASTValue();
        fcg.errored = true;
        return;
    }

    fcg.curBlock->add(std::make_unique<IR::Instrs::Store>(targetDeref->ptr, rhs));

    ret = rhs;
}
void CodeGen::FunctionCodeGen::ExprCodeGen::visitCastExpr(ASTNS::CastExpr *ast)
{
    IR::ASTValue oper = expr(ast->expr.get());
    if (!oper)
    {
        ret = IR::ASTValue();
        return;
    }

    ret = cg.typeVisitor->type(ast->type.get())->castTo(*cg.context, *fcg.fun, fcg.curBlock, oper, ast);

    if (!ret)
        fcg.errored = true;
}

void CodeGen::FunctionCodeGen::ExprCodeGen::visitBlock(ASTNS::Block *ast)
{
    fcg.incScope();
    if (ast->stmts)
        fcg.stmtCG.stmt(ast->stmts.get());

    if (ast->implRet)
        ast->implRet->accept(this);
    else
        ret = IR::ASTValue(cg.context->getVoid(), ast);;
    fcg.decScope();
}
void CodeGen::FunctionCodeGen::ExprCodeGen::visitImplRet(ASTNS::ImplRet *ast)
{
    ret = expr(ast->expr.get());
}

