#include "codegen/codegen.h"
#include "message/errors.h"
#include "message/reportAbort.h"
#include "message/errmsgs.h"

CodeGenNS::ExprCodeGen::ExprCodeGen(CodeGen &cg): cg(cg) {}

IR::ASTValue CodeGenNS::ExprCodeGen::expr(ASTNS::ExprB *ast)
{
    ret = IR::ASTValue();
    ast->accept(this);
    return ret;
}

#define BINARYOPSTART()                      \
    IR::ASTValue lhs = expr(ast->lhs.get()); \
    IR::ASTValue rhs = expr(ast->rhs.get()); \
    if (!lhs || !rhs)                        \
    {                                        \
        ret = IR::ASTValue();                \
        return;                              \
    }
#define BINARYOPEND()                                                  \
    ret = lhs.type()->binOp(cg.context, oper, lhs, rhs, ast->op, ast); \
    if (!ret)                                                          \
        cg.errored = true;

#define BINARYOPSWITCH() IR::Type::BinaryOperator oper; switch (ast->op.type) {
#define BINARYOPSWITCHEND(astty)              \
        default: invalidTok(#astty, ast->op); \
    }
#define BINARYOPCASE(tokentype, optype) case TokenType::tokentype: oper = IR::Type::BinaryOperator::optype; break;

#define BINARYOPIS(operty) IR::Type::BinaryOperator oper = IR::Type::BinaryOperator::operty;

void CodeGenNS::ExprCodeGen::visitBinAndExpr(ASTNS::BinAndExpr *ast)
{
    BINARYOPSTART()
    BINARYOPIS(doubleamper)
    BINARYOPEND()
}
void CodeGenNS::ExprCodeGen::visitBinOrExpr(ASTNS::BinOrExpr *ast)
{
    BINARYOPSTART()
    BINARYOPIS(doublepipe)
    BINARYOPEND()
}
void CodeGenNS::ExprCodeGen::visitBitAndExpr(ASTNS::BitAndExpr *ast)
{
    BINARYOPSTART()
    BINARYOPIS(amper)
    BINARYOPEND()
}
void CodeGenNS::ExprCodeGen::visitBitOrExpr(ASTNS::BitOrExpr *ast)
{
    BINARYOPSTART()
    BINARYOPIS(pipe)
    BINARYOPEND()
}
void CodeGenNS::ExprCodeGen::visitBitShiftExpr(ASTNS::BitShiftExpr *ast)
{
    BINARYOPSTART()
    BINARYOPSWITCH()
    BINARYOPCASE(DOUBLEGREATER, doublegreater)
    BINARYOPCASE(DOUBLELESS, doubleless)
    BINARYOPSWITCHEND(BitshiftExpr operator)
    BINARYOPEND()
}
void CodeGenNS::ExprCodeGen::visitBitXorExpr(ASTNS::BitXorExpr *ast)
{
    BINARYOPSTART()
    BINARYOPIS(caret);
    BINARYOPEND()
}
void CodeGenNS::ExprCodeGen::visitCompEQExpr(ASTNS::CompEQExpr *ast)
{
    BINARYOPSTART()
    BINARYOPSWITCH()
    BINARYOPCASE(BANGEQUAL, bangequal)
    BINARYOPCASE(DOUBLEEQUAL, doubleequal)
    BINARYOPSWITCHEND(CompeqExpr operator)
    BINARYOPEND()
}
void CodeGenNS::ExprCodeGen::visitCompLGTExpr(ASTNS::CompLGTExpr *ast)
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
void CodeGenNS::ExprCodeGen::visitMultExpr(ASTNS::MultExpr *ast)
{
    BINARYOPSTART()
    BINARYOPSWITCH()
    BINARYOPCASE(STAR, star)
    BINARYOPCASE(SLASH, slash)
    BINARYOPCASE(PERCENT, percent)
    BINARYOPSWITCHEND(MultExpr operator)
    BINARYOPEND()
}
void CodeGenNS::ExprCodeGen::visitAdditionExpr(ASTNS::AdditionExpr *ast)
{
    BINARYOPSTART()
    BINARYOPSWITCH()
    BINARYOPCASE(PLUS, plus)
    BINARYOPCASE(MINUS, minus)
    BINARYOPSWITCHEND(AdditionExpr operator)
    BINARYOPEND()
}
#undef BINARYOPSTART
#undef BINARYOPEND
#undef BINARYOPSWITCH
#undef BINARYOPSWITCHEND
#undef BINARYOPCASE
#undef BINARYOPIS

void CodeGenNS::ExprCodeGen::visitUnaryExpr(ASTNS::UnaryExpr *ast)
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

    ret = oper.type()->unaryOp(cg.context, opor, oper, ast->op, ast);
    if (!ret)
        cg.errored = true;
}

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
        ERR_CALL_NONCALLABLE(func, ast->oparn);
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
        ERR_WRONG_NUM_ARGS(func, ast->oparn, ast->args.get(), args);
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
            ERR_INCORRECT_ARG(*i, *j);
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

    IR::TempRegister *outReg = cg.context.curFunc->addTempRegister(retty);

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

    int _intbase;
    switch (ast->value.type)
    {
        case TokenType::TRUELIT:
            ret = IR::ASTValue(cg.context.getConstInt(cg.context.getBuiltinType(IR::BuiltinType::Builtins::BOOL), 1), ast);
            return;

        case TokenType::FALSELIT:
            ret = IR::ASTValue(cg.context.getConstInt(cg.context.getBuiltinType(IR::BuiltinType::Builtins::BOOL), 0), ast);
            return;

        case TokenType::FLOATLIT:
            reportAbortNoh("floating point literals are not supported yet");

        case TokenType::NULLPTRLIT:
            reportAbortNoh("nullptr literals are not supported yet");

        case TokenType::DECINTLIT:
            ret = IR::ASTValue(cg.context.getConstInt(cg.context.getBuiltinType(IR::BuiltinType::Builtins::UINT32), std::stoll(ast->value.stringify())), ast);
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
            ret = IR::ASTValue(cg.context.getConstInt(cg.context.getBuiltinType(IR::BuiltinType::Builtins::UINT32), std::stoll(ast->value.stringify().erase(0, 2), nullptr, _intbase)), ast);
            return;

        case TokenType::CHARLIT:
            ret = IR::ASTValue(cg.context.getConstInt(cg.context.getBuiltinType(IR::BuiltinType::Builtins::CHAR), *(ast->value.start + 1)), ast);
            return;

        case TokenType::STRINGLIT:
            reportAbortNoh("string literals are not supported yet");

        case TokenType::IDENTIFIER:
            {
                IR::Value *v = cg.context.findValue(ast->value.stringify());
                if (!v)
                {
                    ERR_UNDECL_SYMB(ast->value);
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
void CodeGenNS::ExprCodeGen::visitIfExpr(ASTNS::IfExpr *ast)
{
    IR::ASTValue cond = expr(ast->cond.get());
    if (!cond)
    {
        ret = IR::ASTValue();
        return;
    }
    cond = cond.type()->isTrue(cg.context, cond);
    if (!cond)
    {
        cg.errored = true;
        ret = IR::ASTValue();
        return;
    }

    IR::Block *trueb  = cg.context.curFunc->addBlock("if_true");
    IR::Block *falseb = ast->falses ? cg.context.curFunc->addBlock("if_false") : nullptr;
    IR::Block *afterb = cg.context.curFunc->addBlock("if_after");

    if (falseb)
        cg.context.curBlock->branch(std::make_unique<IR::Instrs::CondBr>(cond, trueb, falseb));
    else
        cg.context.curBlock->branch(std::make_unique<IR::Instrs::CondBr>(cond, trueb, afterb));

    cg.context.curBlock = trueb;
    IR::ASTValue truev = expr(ast->trues.get());
    if (!truev)
    {
        ret = IR::ASTValue();
        return;
    }
    cg.context.curBlock->branch(std::make_unique<IR::Instrs::GotoBr>(afterb));
    trueb = cg.context.curBlock;

    IR::ASTValue falsev;
    if (falseb)
    {
        cg.context.curBlock = falseb;
        falsev = expr(ast->falses.get());
        if (!falsev)
        {
            ret = IR::ASTValue();
            return;
        }
        cg.context.curBlock->branch(std::make_unique<IR::Instrs::GotoBr>(afterb));
        falseb = cg.context.curBlock;
    }

    if (falseb)
    {
        if (truev.type() != falsev.type())
        {
            ERR_CONFL_TYS_IFEXPR(truev, falsev, ast->iftok);
            ret = IR::ASTValue();
            cg.errored = true;
            return;
        }
    }
    else
    {
        if (!dynamic_cast<IR::VoidType*>(truev.type()))
        {
            ERR_NO_ELSE_NOT_VOID(truev, ast->iftok);
            ret = IR::ASTValue();
            cg.errored = true;
            return;
        }
    }

    cg.context.curBlock = afterb;

    if (falseb)
    {
        IR::TempRegister *outreg = cg.context.curFunc->addTempRegister(truev.type());
        afterb->add(std::make_unique<IR::Instrs::Phi>(outreg, std::vector {std::make_pair(trueb, truev), std::make_pair(falseb, falsev)}));
        ret = IR::ASTValue(outreg, ast);
    }
    else
        ret = IR::ASTValue(truev.val, ast);

}
void CodeGenNS::ExprCodeGen::visitForExpr(ASTNS::ForExpr *ast)
{
    cg.context.incScope();

    cg.stmtCodeGen.stmt(ast->start.get());

    IR::Block *loopCheckCond = cg.context.curFunc->addBlock("loop_checkcond");
    IR::Block *loopBody = cg.context.curFunc->addBlock("loop_body");
    IR::Block *loopAfter = cg.context.curFunc->addBlock("loop_after");

    cg.context.curBlock->branch(std::make_unique<IR::Instrs::GotoBr>(loopCheckCond));
    cg.context.curBlock = loopCheckCond;
    IR::ASTValue cond = expr(ast->cond.get());
    loopCheckCond->branch(std::make_unique<IR::Instrs::CondBr>(cond, loopBody, loopAfter));

    cg.context.curBlock = loopBody;
    expr(ast->body.get());

    expr(ast->increment.get());
    loopBody->branch(std::make_unique<IR::Instrs::GotoBr>(loopCheckCond));
    cg.context.decScope();

    cg.context.curBlock = loopAfter;

    ret = IR::ASTValue(cg.context.getVoidValue(), ast);
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

    if (!targetReg)
    {
        ERR_ASSIGN_INVALID_LHS(ast->equal, lhs);
        ret = IR::ASTValue();
        cg.errored = true;
        return;
    }

    if (targetReg->type() != rhs.type())
    {
        ERR_ASSIGN_CONFLICT_TYS(lhs, rhs, ast->equal);
        ret = IR::ASTValue();
        cg.errored = true;
        return;
    }

    cg.context.curBlock->add(std::make_unique<IR::Instrs::Store>(targetReg, rhs));

    ret = rhs;
}
void CodeGenNS::ExprCodeGen::visitCastExpr(ASTNS::CastExpr *ast)
{
    IR::ASTValue oper = expr(ast->operand.get());

    ret = cg.typeResolver.type(ast->type.get())->castTo(cg.context, oper, ast);

    if (!ret)
        cg.errored = true;
}

void CodeGenNS::ExprCodeGen::visitIndentedBlock(ASTNS::IndentedBlock *ast)
{
    cg.context.incScope();
    if (ast->stmts)
        ast->stmts->accept(&cg.stmtCodeGen);

    ret = expr(ast->implret.get());
    cg.context.decScope();
    if (!ret)
        ret = IR::ASTValue(cg.context.getVoidValue(), ast);
}
void CodeGenNS::ExprCodeGen::visitBracedBlock(ASTNS::BracedBlock *ast)
{
    cg.context.incScope();
    if (ast->stmts)
        ast->stmts->accept(&cg.stmtCodeGen);

    ret = expr(ast->implret.get());
    cg.context.decScope();
    if (!ret)
        ret = IR::ASTValue(cg.context.getVoidValue(), ast);
}
void CodeGenNS::ExprCodeGen::visitImplRet(ASTNS::ImplRet *ast)
{
    ret = expr(ast->expr.get());
}
void CodeGenNS::ExprCodeGen::visitImplRet_OPT(ASTNS::ImplRet_OPT *ast)
{
    ret = IR::ASTValue();
}

void CodeGenNS::ExprCodeGen::visitExpr_OPT(ASTNS::Expr_OPT *ast) {}
