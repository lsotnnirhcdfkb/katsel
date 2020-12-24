#include "codegenlocal.h"
#include "message/internal.h"
#include "message/reportAbort.h"
#include "message/errmsgs.h"

CodeGen::FunctionCodeGen::ExprCodeGen::ExprCodeGen(CodeGen &cg, FunctionCodeGen &fcg): cg(cg), fcg(fcg) {}

IR::ASTValue CodeGen::FunctionCodeGen::ExprCodeGen::expr(ASTNS::ExprB *ast)
{
    ret = IR::ASTValue();
    ast->accept(this);
    return ret;
}

// binary operations {{{1
#define BINARYOPSTART()                      \
    IR::ASTValue lhs = expr(ast->lhs.get()); \
    IR::ASTValue rhs = expr(ast->rhs.get()); \
    if (!lhs || !rhs)                        \
    {                                        \
        ret = IR::ASTValue();                \
        return;                              \
    }
#define BINARYOPEND()                                                                           \
    ret = lhs.type()->binOp(*cg.context, *fcg.fun, fcg.curBlock, oper, lhs, rhs, ast->op, ast); \
    if (!ret)                                                                                   \
        fcg.errored = true;

#define BINARYOPSWITCH() IR::Type::BinaryOperator oper; switch (ast->op.type) {
#define BINARYOPSWITCHEND(astty)              \
        default: invalidTok(#astty, ast->op); \
    }
#define BINARYOPCASE(tokentype, optype) case TokenType::tokentype: oper = IR::Type::BinaryOperator::optype; break;

#define BINARYOPIS(operty) IR::Type::BinaryOperator oper = IR::Type::BinaryOperator::operty;

void CodeGen::FunctionCodeGen::ExprCodeGen::visitBitAndExpr(ASTNS::BitAndExpr *ast)
{
    BINARYOPSTART()
    BINARYOPIS(amper)
    BINARYOPEND()
}
void CodeGen::FunctionCodeGen::ExprCodeGen::visitBitOrExpr(ASTNS::BitOrExpr *ast)
{
    BINARYOPSTART()
    BINARYOPIS(pipe)
    BINARYOPEND()
}
void CodeGen::FunctionCodeGen::ExprCodeGen::visitBitShiftExpr(ASTNS::BitShiftExpr *ast)
{
    BINARYOPSTART()
    BINARYOPSWITCH()
    BINARYOPCASE(DOUBLEGREATER, doublegreater)
    BINARYOPCASE(DOUBLELESS, doubleless)
    BINARYOPSWITCHEND(BitshiftExpr operator)
    BINARYOPEND()
}
void CodeGen::FunctionCodeGen::ExprCodeGen::visitBitXorExpr(ASTNS::BitXorExpr *ast)
{
    BINARYOPSTART()
    BINARYOPIS(caret);
    BINARYOPEND()
}
void CodeGen::FunctionCodeGen::ExprCodeGen::visitCompEQExpr(ASTNS::CompEQExpr *ast)
{
    BINARYOPSTART()
    BINARYOPSWITCH()
    BINARYOPCASE(BANGEQUAL, bangequal)
    BINARYOPCASE(DOUBLEEQUAL, doubleequal)
    BINARYOPSWITCHEND(CompeqExpr operator)
    BINARYOPEND()
}
void CodeGen::FunctionCodeGen::ExprCodeGen::visitCompLGTExpr(ASTNS::CompLGTExpr *ast)
{
    BINARYOPSTART()
    BINARYOPSWITCH()
    BINARYOPCASE(LESS, less)
    BINARYOPCASE(GREATER, greater)
    BINARYOPCASE(LESSEQUAL, lessequal)
    BINARYOPCASE(GREATEREQUAL, greaterequal)
    BINARYOPSWITCHEND(ComplgtExpr operator)
    BINARYOPEND()
}
void CodeGen::FunctionCodeGen::ExprCodeGen::visitMultExpr(ASTNS::MultExpr *ast)
{
    BINARYOPSTART()
    BINARYOPSWITCH()
    BINARYOPCASE(STAR, star)
    BINARYOPCASE(SLASH, slash)
    BINARYOPCASE(PERCENT, percent)
    BINARYOPSWITCHEND(MultExpr operator)
    BINARYOPEND()
}
void CodeGen::FunctionCodeGen::ExprCodeGen::visitAdditionExpr(ASTNS::AdditionExpr *ast)
{
    BINARYOPSTART()
    BINARYOPSWITCH()
    BINARYOPCASE(PLUS, plus)
    BINARYOPCASE(MINUS, minus)
    BINARYOPSWITCHEND(AdditionExpr operator)
    BINARYOPEND()
}
// }}}
// short circuit operations {{{
void CodeGen::FunctionCodeGen::ExprCodeGen::visitBinAndExpr(ASTNS::BinAndExpr *ast)
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

    IR::Block *lfalseb = fcg.fun->addBlock("binaryand_lfalseb");
    IR::Block *checkbothb = fcg.fun->addBlock("binaryand_checkbothb");
    IR::Block *afterb = fcg.fun->addBlock("binaryand_afterb");

    // i && j
    // becomes
    // if (i)
    //     j
    //  else
    //     false

    fcg.curBlock->branch(std::make_unique<IR::Instrs::CondBr>(lhs, checkbothb, lfalseb));

    fcg.curBlock = checkbothb;
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
    fcg.curBlock->branch(std::make_unique<IR::Instrs::GotoBr>(afterb));

    fcg.curBlock = lfalseb;
    fcg.curBlock->branch(std::make_unique<IR::Instrs::GotoBr>(afterb));

    fcg.curBlock = afterb;
    IR::TempRegister *retReg = fcg.fun->addTempRegister(cg.context->getBoolType());
    fcg.curBlock->add(std::make_unique<IR::Instrs::Phi>(retReg, std::vector {std::make_pair(checkbothb, rhs), std::make_pair(lfalseb, IR::ASTValue(cg.context->getConstBool(false), ast))}));
}
void CodeGen::FunctionCodeGen::ExprCodeGen::visitBinOrExpr(ASTNS::BinOrExpr *ast)
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

    IR::Block *ltrueb = fcg.fun->addBlock("binaryor_ltrueb");
    IR::Block *checkbothb = fcg.fun->addBlock("binaryor_checkbothb");
    IR::Block *afterb = fcg.fun->addBlock("binaryor_afterb");

    // i || j
    // becomes
    // if (i)
    //     true
    //  else
    //     j

    fcg.curBlock->branch(std::make_unique<IR::Instrs::CondBr>(lhs, ltrueb, checkbothb));

    fcg.curBlock = ltrueb;
    fcg.curBlock->branch(std::make_unique<IR::Instrs::GotoBr>(afterb));

    fcg.curBlock = checkbothb;
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
    fcg.curBlock->branch(std::make_unique<IR::Instrs::GotoBr>(afterb));

    fcg.curBlock = afterb;
    IR::TempRegister *retReg = fcg.fun->addTempRegister(cg.context->getBoolType());
    fcg.curBlock->add(std::make_unique<IR::Instrs::Phi>(retReg, std::vector {std::make_pair(ltrueb, IR::ASTValue(cg.context->getConstBool(true), ast)), std::make_pair(checkbothb, rhs)}));
}
// }}}
#undef BINARYOPSTART
#undef BINARYOPEND
#undef BINARYOPSWITCH
#undef BINARYOPSWITCHEND
#undef BINARYOPCASE
#undef BINARYOPIS

void CodeGen::FunctionCodeGen::ExprCodeGen::visitUnaryExpr(ASTNS::UnaryExpr *ast)
{
    IR::ASTValue oper = expr(ast->operand.get());
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

    IR::Type *retty = fty->ret;
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

    IR::TempRegister *outReg = fcg.fun->addTempRegister(retty);

    fcg.curBlock->add(std::make_unique<IR::Instrs::Call>(outReg, static_cast<IR::Function *>(func.val), args));

    ret = IR::ASTValue(outReg, ast);
}

void CodeGen::FunctionCodeGen::ExprCodeGen::visitPrimaryExpr(ASTNS::PrimaryExpr *ast)
{
    if (ast->form == ASTNS::PrimaryExpr::Form::TAT)
    {
        ast->expr->accept(this);
        return;
    }

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
                    v = l->v;

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
        IR::TempRegister *outreg = fcg.fun->addTempRegister(truev.type());
        afterb->add(std::make_unique<IR::Instrs::Phi>(outreg, std::vector {std::make_pair(trueb, truev), std::make_pair(falseb, falsev)}));
        ret = IR::ASTValue(outreg, ast);
    }
    else
        ret = IR::ASTValue(truev.val, ast);

}
void CodeGen::FunctionCodeGen::ExprCodeGen::visitForExpr(ASTNS::ForExpr *ast)
{
    fcg.incScope();

    fcg.stmtCG.stmt(ast->start.get());

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
    IR::ASTValue rhs = expr(ast->value.get());

    if (!lhs || !rhs)
    {
        ret = IR::ASTValue();
        return;
    }

    IR::Register *targetReg = dynamic_cast<IR::Register*>(lhs.val);

    if (!targetReg)
    {
        ERR_ASSIGN_INVALID_LHS(ast->equal, lhs);
        ret = IR::ASTValue();
        fcg.errored = true;
        return;
    }

    rhs = targetReg->type()->implCast(*cg.context, *fcg.fun, fcg.curBlock, rhs);
    if (targetReg->type() != rhs.type())
    {
        ERR_ASSIGN_CONFLICT_TYS(lhs, rhs, ast->equal);
        ret = IR::ASTValue();
        fcg.errored = true;
        return;
    }

    fcg.curBlock->add(std::make_unique<IR::Instrs::Store>(targetReg, rhs));

    ret = rhs;
}
void CodeGen::FunctionCodeGen::ExprCodeGen::visitCastExpr(ASTNS::CastExpr *ast)
{
    IR::ASTValue oper = expr(ast->operand.get());
    if (!oper)
    {
        ret = IR::ASTValue();
        return;
    }

    ret = cg.typeVisitor->type(ast->type.get())->castTo(*cg.context, *fcg.fun, fcg.curBlock, oper, ast);

    if (!ret)
        fcg.errored = true;
}

void CodeGen::FunctionCodeGen::ExprCodeGen::visitIndentedBlock(ASTNS::IndentedBlock *ast)
{
    fcg.incScope();
    if (ast->stmts)
        fcg.stmtCG.stmt(ast->stmts.get());

    ret = expr(ast->implret.get());
    fcg.decScope();
}
void CodeGen::FunctionCodeGen::ExprCodeGen::visitBracedBlock(ASTNS::BracedBlock *ast)
{
    fcg.incScope();
    if (ast->stmts)
        fcg.stmtCG.stmt(ast->stmts.get());

    ret = expr(ast->implret.get());
    fcg.decScope();
}
void CodeGen::FunctionCodeGen::ExprCodeGen::visitImplRet(ASTNS::ImplRet *ast)
{
    ret = expr(ast->expr.get());
}
void CodeGen::FunctionCodeGen::ExprCodeGen::visitImplRet_OPT(ASTNS::ImplRet_OPT *ast)
{
    ret = IR::ASTValue(cg.context->getVoid(), ast);;
}

void CodeGen::FunctionCodeGen::ExprCodeGen::visitExpr_OPT(ASTNS::Expr_OPT *ast) {}
