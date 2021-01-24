#include "codegenlocal.h"
#include "message/internal.h"
#include "message/reportAbort.h"
#include "message/errmsgs.h"
#include "ir/unit.h"
#include "ast/ast.h"
#include "ir/instruction.h"

CodeGen::FunctionCodeGen::ExprCodeGen::ExprCodeGen(CodeGen &cg, FunctionCodeGen &fcg): cg(cg), fcg(fcg) {}

Maybe<IR::ASTValue> CodeGen::FunctionCodeGen::ExprCodeGen::expr(NNPtr<ASTNS::Expr> ast) {
    ret = Maybe<IR::ASTValue>();
    ast->accept(*this);
    return ret;
}

void CodeGen::FunctionCodeGen::ExprCodeGen::visit_binary_expr(ASTNS::BinaryExpr &ast) {
    Maybe<IR::ASTValue> m_lhs = expr(ast.lhs.get());
    Maybe<IR::ASTValue> m_rhs = expr(ast.rhs.get());
    if (!m_lhs.has() || !m_rhs.has()) {
        ret = Maybe<IR::ASTValue>();
        return;
    }

    IR::ASTValue lhs = m_lhs.get(), rhs = m_rhs.get();

    IR::Type::BinaryOperator oper;
    switch (ast.op.type) {
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
        default: invalidTok("binary operator", ast.op);
    }
    ret = lhs.type()->binOp(*cg.context, *fcg.fun, fcg.curBlock, oper, lhs, rhs, ast.op, ast);
    if (!ret.has())
        fcg.errored = true;
}
void CodeGen::FunctionCodeGen::ExprCodeGen::visit_short_circuit_expr(ASTNS::ShortCircuitExpr &ast) {
    Maybe<IR::ASTValue> m_lhs = expr(ast.lhs.get());
    if (!m_lhs.has()) {
        ret = Maybe<IR::ASTValue>();
        return;
    }

    IR::ASTValue lhs = m_lhs.get();

    if (!dynamic_cast<IR::BoolType*>(lhs.type().asRaw())) {
        ERR_LHS_UNSUPPORTED_OP(lhs, ast.op);
        ret = Maybe<IR::ASTValue>();
        fcg.errored = true;
        return;
    }

    NNPtr<IR::Block> skip = fcg.fun->addBlock("shortcircuit_skip");
    NNPtr<IR::Block> checkboth = fcg.fun->addBlock("shortcircuit_checkboth");
    NNPtr<IR::Block> after = fcg.fun->addBlock("shortcircuit_after");

    bool valueIfSkipped;
    if (ast.op.type == TokenType::DOUBLEPIPE) {
        // jump to skip when true
        fcg.curBlock->branch(std::make_unique<IR::Instrs::CondBr>(lhs, skip, checkboth));
        valueIfSkipped = true;
    } else if (ast.op.type == TokenType::DOUBLEAMPER) {
        // jump to skip when false
        fcg.curBlock->branch(std::make_unique<IR::Instrs::CondBr>(lhs, checkboth, skip));
        valueIfSkipped = false;
    } else
        invalidTok("short circuiting operator", ast.op);

    fcg.curBlock = checkboth;

    Maybe<IR::ASTValue> m_rhs = expr(ast.rhs.get());
    if (!m_rhs.has()) {
        ret = Maybe<IR::ASTValue>();
        return;
    }

    IR::ASTValue rhs = m_rhs.get();

    if (!dynamic_cast<IR::BoolType*>(rhs.type().asRaw())) {
        ERR_CONFLICT_TYS_BINARY_OP(lhs, rhs, ast.op);
        ret = Maybe<IR::ASTValue>();
        fcg.errored = true;
        return;
    }
    fcg.curBlock->branch(std::make_unique<IR::Instrs::GotoBr>(after));

    fcg.curBlock = skip;
    fcg.curBlock->branch(std::make_unique<IR::Instrs::GotoBr>(after));

    fcg.curBlock = after;
    ret = IR::ASTValue(fcg.curBlock->add(std::make_unique<IR::Instrs::Phi>(std::vector {std::make_pair(checkboth, rhs), std::make_pair(skip, IR::ASTValue(cg.context->getConstBool(valueIfSkipped), ast))})), ast);
}

void CodeGen::FunctionCodeGen::ExprCodeGen::visit_unary_expr(ASTNS::UnaryExpr &ast) {
    Maybe<IR::ASTValue> m_oper = expr(ast.expr.get());
    if (!m_oper.has()) {
        ret = Maybe<IR::ASTValue>();
        return;
    }

    IR::ASTValue oper = m_oper.get();

    IR::Type::UnaryOperator opor;

    switch (ast.op.type) {
        case TokenType::TILDE:
            opor = IR::Type::UnaryOperator::tilde; break;
        case TokenType::MINUS:
            opor = IR::Type::UnaryOperator::minus; break;
        case TokenType::BANG:
            opor = IR::Type::UnaryOperator::bang; break;
        default:
            invalidTok("unary operator", ast.op);
    }

    ret = oper.type()->unaryOp(*cg.context, *fcg.fun, fcg.curBlock, opor, oper, ast.op, ast);
    if (!ret.has())
        fcg.errored = true;
}

void CodeGen::FunctionCodeGen::ExprCodeGen::visit_deref_expr(ASTNS::DerefExpr &ast) {
    Maybe<IR::ASTValue> m_oper = expr(ast.expr.get());
    if (!m_oper.has()) {
        ret = Maybe<IR::ASTValue>();
        return;
    }

    IR::ASTValue oper = m_oper.get();

    IR::PointerType *asptrty (dynamic_cast<IR::PointerType*>(oper.type().asRaw()));
    if (!asptrty) {
        ERR_NO_DEREF(ast.op, oper);
        ret = Maybe<IR::ASTValue>();
        fcg.errored = true;
        return;
    }

    ret = IR::ASTValue(fcg.curBlock->add(std::make_unique<IR::Instrs::DerefPtr>(oper)), ast);
}
void CodeGen::FunctionCodeGen::ExprCodeGen::visit_addrof_expr(ASTNS::AddrofExpr &ast) {
    Maybe<IR::ASTValue> m_oper = expr(ast.expr.get());
    if (!m_oper.has()) {
        ret = Maybe<IR::ASTValue>();
        return;
    }

    IR::ASTValue oper = m_oper.get();

    IR::Instrs::DerefPtr *asDeref = dynamic_cast<IR::Instrs::DerefPtr*>(oper.val.asRaw());
    if (!asDeref) {
        ERR_ADDROF_NOT_LVALUE(ast.op, oper);
        ret = Maybe<IR::ASTValue>();
        fcg.errored = true;
        return;
    }

    if (static_cast<IR::PointerType*>(asDeref->ptr.type().asRaw())->mut == false && ast.mut) {
        ERR_MUT_ADDROF_NONMUT_OP(ast.op, asDeref);
        ret = Maybe<IR::ASTValue>();
        fcg.errored = true;
        return;
    }

    ret = IR::ASTValue(fcg.curBlock->add(std::make_unique<IR::Instrs::Addrof>(asDeref, ast.mut)), ast);
}

void CodeGen::FunctionCodeGen::ExprCodeGen::visit_call_expr(ASTNS::CallExpr &ast) {
    Maybe<IR::ASTValue> m_fun = expr(ast.callee.get());
    if (!m_fun.has()) {
        ret = Maybe<IR::ASTValue>();
        return;
    }

    IR::ASTValue fun = m_fun.get();

    IR::FunctionType *fty = dynamic_cast<IR::FunctionType*>(fun.type().asRaw());
    if (!fty) {
        ERR_CALL_NONCALLABLE(fun, ast.oparn);
        ret = Maybe<IR::ASTValue>();
        fcg.errored = true;
        return;
    }

    CodeGen::ArgVisitor av (fcg, ast.args);
    std::vector<IR::ASTValue> args (av.ret);

    if (args.size() != fty->paramtys.size()) {
        ERR_WRONG_NUM_ARGS(static_cast<IR::Function*>(fun.val.asRaw()), ast.callee.get(), ast.oparn, args);
        ret = Maybe<IR::ASTValue>();
        fcg.errored = true;
        return;
    }

    bool argserr = false;
    auto i = args.begin();
    auto j = fty->paramtys.begin();
    for (; i != args.end() && j != fty->paramtys.end(); ++i, ++j) {
        *i = (*j)->implCast(*cg.context, *fcg.fun, fcg.curBlock, *i);
        if (i->type() != *j) {
            ERR_INCORRECT_ARG(*i, *j);
            ret = Maybe<IR::ASTValue>();
            fcg.errored = true;
            argserr = true;
        }
    }

    if (argserr) {
        ret = Maybe<IR::ASTValue>();
        return;
    }

    ret = IR::ASTValue(fcg.curBlock->add(std::make_unique<IR::Instrs::Call>(NNPtr<IR::Function>(static_cast<IR::Function*>(fun.val.asRaw())), args)), ast);
}

void CodeGen::FunctionCodeGen::ExprCodeGen::visit_primary_expr(ASTNS::PrimaryExpr &ast) {
    int _intbase;
    switch (ast.value.type) {
        case TokenType::TRUELIT:
            ret = IR::ASTValue(cg.context->getConstBool(true), ast);
            return;

        case TokenType::FALSELIT:
            ret = IR::ASTValue(cg.context->getConstBool(false), ast);
            return;

        case TokenType::FLOATLIT:
            ret = IR::ASTValue(cg.context->getConstFloat(cg.context->getGenericFloatType(), std::stod(ast.value.stringify())), ast);
            return;

        case TokenType::NULLPTRLIT:
            reportAbortNoh("nullptr literals are not supported yet");

        case TokenType::DECINTLIT:
            ret = IR::ASTValue(cg.context->getConstInt(cg.context->getGenericIntType(), std::stoll(ast.value.stringify())), ast);
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
            ret = IR::ASTValue(cg.context->getConstInt(cg.context->getGenericIntType(), std::stoll(ast.value.stringify().erase(0, 2), nullptr, _intbase)), ast);
            return;

        case TokenType::CHARLIT:
            ret = IR::ASTValue(cg.context->getConstChar(*(ast.value.start + 1)), ast);
            return;

        case TokenType::STRINGLIT:
            reportAbortNoh("string literals are not supported yet");

        case TokenType::THIS: {
                Maybe<NNPtr<Local>> m_loc = fcg.getLocal("this");
                if (m_loc.has()) {
                    NNPtr<Local> local = m_loc.get();
                    ret = IR::ASTValue(fcg.curBlock->add(std::make_unique<IR::Instrs::DerefPtr>(IR::ASTValue(local->v, ast))), ast);
                } else {
                    ERR_NO_THIS(ast.value);
                    fcg.errored = true;
                    ret = Maybe<IR::ASTValue>();
                }
                return;
            }

        default:
            invalidTok("primary token", ast.value);
    }
}
void CodeGen::FunctionCodeGen::ExprCodeGen::visit_if_expr(ASTNS::IfExpr &ast) {
    Maybe<IR::ASTValue> m_cond = expr(ast.cond.get());
    if (!m_cond.has()) {
        ret = Maybe<IR::ASTValue>();
        return;
    }
    IR::ASTValue cond = m_cond.get();

    if (!dynamic_cast<IR::BoolType*>(cond.type().asRaw())) {
        ERR_COND_NOT_BOOL(cond);
        ret = Maybe<IR::ASTValue>();
        fcg.errored = true;
        return;
    }

    NNPtr<IR::Block> trueb  = fcg.fun->addBlock("if_true");
    Maybe<NNPtr<IR::Block>> falseb = ast.falses ? Maybe<NNPtr<IR::Block>>(fcg.fun->addBlock("if_false")) : Maybe<NNPtr<IR::Block>>();
    NNPtr<IR::Block> afterb = fcg.fun->addBlock("if_after");

    if (falseb.has())
        fcg.curBlock->branch(std::make_unique<IR::Instrs::CondBr>(cond, trueb, falseb.get()));
    else
        fcg.curBlock->branch(std::make_unique<IR::Instrs::CondBr>(cond, trueb, afterb));

    fcg.curBlock = trueb;
    Maybe<IR::ASTValue> m_truev = expr(ast.trues.get());
    if (!m_truev.has()) {
        ret = Maybe<IR::ASTValue>();
        return;
    }

    IR::ASTValue truev = m_truev.get();

    fcg.curBlock->branch(std::make_unique<IR::Instrs::GotoBr>(afterb));
    trueb = fcg.curBlock;

    Maybe<IR::ASTValue> falsev;
    if (falseb.has()) {
        fcg.curBlock = falseb.get();

        Maybe<IR::ASTValue> m_falsev = expr(ast.falses.get());
        if (!m_falsev.has()) {
            ret = Maybe<IR::ASTValue>();
            return;
        }

        falsev = m_falsev;

        fcg.curBlock->branch(std::make_unique<IR::Instrs::GotoBr>(afterb));
        falseb = fcg.curBlock;

        // try both implicit casts -- they are mostly asymmetrical so it should be fine
        truev = falsev.get().type()->implCast(*cg.context, *fcg.fun, fcg.curBlock, truev);
        falsev = truev.type()->implCast(*cg.context, *fcg.fun, fcg.curBlock, falsev.get());

        if (truev.type() != falsev.get().type()) {
            ERR_CONFL_TYS_IFEXPR(truev, falsev.get(), ast.iftok, ast.elsetok);
            ret = Maybe<IR::ASTValue>();
            fcg.errored = true;
            return;
        }
    } else {
        if (!dynamic_cast<IR::VoidType*>(truev.type().asRaw())) {
            ERR_NO_ELSE_NOT_VOID(truev, ast.iftok);
            ret = Maybe<IR::ASTValue>();
            fcg.errored = true;
            return;
        }
    }

    fcg.curBlock = afterb;

    if (falseb.has()) {
        ret = IR::ASTValue(afterb->add(std::make_unique<IR::Instrs::Phi>(std::vector({ std::make_pair(trueb, truev), std::make_pair(falseb.get(), falsev.get()) }))), ast);
    } else
        ret = IR::ASTValue(truev.val, ast);

}
void CodeGen::FunctionCodeGen::ExprCodeGen::visit_while_expr(ASTNS::WhileExpr &ast) {
    fcg.incScope();

    NNPtr<IR::Block> loopCheckCond = fcg.fun->addBlock("loop_checkcond");
    NNPtr<IR::Block> loopBody = fcg.fun->addBlock("loop_body");
    NNPtr<IR::Block> loopAfter = fcg.fun->addBlock("loop_after");

    fcg.curBlock->branch(std::make_unique<IR::Instrs::GotoBr>(loopCheckCond));
    fcg.curBlock = loopCheckCond;

    Maybe<IR::ASTValue> m_cond = expr(ast.cond.get());
    if (!m_cond.has()) {
        ret = Maybe<IR::ASTValue>();
        return;
    }

    IR::ASTValue cond = m_cond.get();

    if (!dynamic_cast<IR::BoolType*>(cond.type().asRaw())) {
        ERR_COND_NOT_BOOL(cond);
        ret = Maybe<IR::ASTValue>();
        fcg.errored = true;
        return;
    }
    fcg.curBlock->branch(std::make_unique<IR::Instrs::CondBr>(cond, loopBody, loopAfter));

    fcg.curBlock = loopBody;
    expr(ast.body.get());

    fcg.curBlock->branch(std::make_unique<IR::Instrs::GotoBr>(loopCheckCond));

    fcg.decScope();

    fcg.curBlock = loopAfter;

    ret = IR::ASTValue(cg.context->getVoid(), ast);
}

void CodeGen::FunctionCodeGen::ExprCodeGen::visit_assignment_expr(ASTNS::AssignmentExpr &ast) {
    Maybe<IR::ASTValue> m_lhs = expr(ast.target.get());
    Maybe<IR::ASTValue> m_rhs = expr(ast.expr.get());

    if (!m_lhs.has() || !m_rhs.has()) {
        ret = Maybe<IR::ASTValue>();
        return;
    }

    IR::ASTValue lhs = m_lhs.get();
    IR::ASTValue rhs = m_rhs.get();

    IR::Instrs::DerefPtr *targetDeref = dynamic_cast<IR::Instrs::DerefPtr*>(lhs.val.asRaw());

    if (!targetDeref) {
        ERR_ASSIGN_INVALID_LHS(ast.equal, lhs);
        ret = Maybe<IR::ASTValue>();
        fcg.errored = true;
        return;
    }

    if (!static_cast<IR::PointerType*>(targetDeref->ptr.type().asRaw())->mut) {
        ERR_ASSIGN_NOT_MUT(lhs, ast.equal, targetDeref);
        ret = Maybe<IR::ASTValue>();
        fcg.errored = true;
        return;
    }

    NNPtr<IR::Type> expectType = targetDeref->type();
    rhs = expectType->implCast(*cg.context, *fcg.fun, fcg.curBlock, rhs);
    if (expectType != rhs.type()) {
        ERR_ASSIGN_CONFLICT_TYS(lhs, rhs, ast.equal);
        ret = Maybe<IR::ASTValue>();
        fcg.errored = true;
        return;
    }

    fcg.curBlock->add(std::make_unique<IR::Instrs::Store>(targetDeref->ptr, rhs, false));

    ret = rhs;
}
void CodeGen::FunctionCodeGen::ExprCodeGen::visit_cast_expr(ASTNS::CastExpr &ast) {
    Maybe<IR::ASTValue> m_oper = expr(ast.expr.get());
    if (!m_oper.has()) {
        ret = Maybe<IR::ASTValue>();
        return;
    }

    IR::ASTValue oper = m_oper.get();

    Maybe<NNPtr<IR::Type>> m_castToTy = cg.typeVisitor->type(ast.type.get(), fcg.thisType);
    if (!m_castToTy.has()) {
        fcg.errored = true;
        return;
    }

    NNPtr<IR::Type> castToTy = m_castToTy.get();

    ret = castToTy->castFrom(*cg.context, *fcg.fun, fcg.curBlock, oper, ast);

    if (!ret.has())
        fcg.errored = true;
}

void CodeGen::FunctionCodeGen::ExprCodeGen::visit_block(ASTNS::Block &ast) {
    fcg.incScope();

    Maybe<IR::ASTValue> blockRet;

    for (auto stmt = ast.stmts.begin(); stmt != ast.stmts.end(); ++stmt) {
        if (ASTNS::ExprStmt *exprstmt = dynamic_cast<ASTNS::ExprStmt*>(stmt->get())) {
            bool last = stmt + 1 == ast.stmts.end();

            if (last && !exprstmt->suppress) {
                // if the stmt is the last stmt of the block, and is not suppressed
                blockRet = expr(exprstmt->expr.get());
                if (!blockRet.has()) {
                    fcg.errored = true;
                    return;
                }
            } else {
                // if the stmt does not count as a return value
                // (ie it is not the last stmt, or it is the last stmt and is suppressed)
                expr(exprstmt->expr.get());

                if (!last && exprstmt->suppress) {
                    ERR_NO_SUPPRESS(exprstmt->dot.get());
                    fcg.errored = true;
                }
            }
        } else {
            fcg.stmtCG.stmt(stmt->get());
        }
    }

    NNPtr<ASTNS::AST> voidAST = ast.stmts.size() ? static_cast<NNPtr<ASTNS::AST>>(ast.stmts[ast.stmts.size() - 1].get()) : static_cast<NNPtr<ASTNS::AST>>(ast);

    ret = blockRet.has() ? blockRet.get() : IR::ASTValue(cg.context->getVoid(), voidAST);

    fcg.decScope();
}

void CodeGen::FunctionCodeGen::ExprCodeGen::visit_path_expr(ASTNS::PathExpr &ast) {
    ret = cg.pathVisitor->resolveValue(ast.path.get(), fcg);
    if (!ret.has())
        fcg.errored = true;
}
void CodeGen::FunctionCodeGen::ExprCodeGen::visit_field_access_expr(ASTNS::FieldAccessExpr &ast) {
    Maybe<IR::ASTValue> m_op = expr(ast.operand.get());
    if (!m_op.has()) {
        fcg.errored = true;
        return;
    }

    IR::ASTValue op = m_op.get();

    bool has = op.type()->hasField(ast.field.stringify());
    if (!has) {
        ERR_NO_FIELD(op, ast.field);
        fcg.errored = true;
        return;
    }

    int ind = op.type()->getFieldIndex(ast.field.stringify());
    // TODO: do this
}
void CodeGen::FunctionCodeGen::ExprCodeGen::visit_method_call_expr(ASTNS::MethodCallExpr &ast) {
    Maybe<IR::ASTValue> m_op = expr(ast.operand.get());
    if (!m_op.has()) {
        fcg.errored = true;
        return;
    }

    IR::ASTValue op = m_op.get();

    Maybe<IR::Type::Method const> m_method = op.type()->getMethod(ast.method.stringify());
    if (!m_method.has()) {
        ERR_NO_METHOD(op, ast.method);
        fcg.errored = true;
        return;
    }

    IR::Type::Method method = m_method.get();

    Maybe<IR::ASTValue> m_thisArg = method.thisPtr ? [this, &op, &ast, &method] () -> Maybe<IR::ASTValue> {
        IR::Instrs::DerefPtr *opAsDeref = dynamic_cast<IR::Instrs::DerefPtr*>(op.val.asRaw());
        if (!opAsDeref) {
            ERR_ADDROF_NOT_LVALUE(ast.dot, op);
            fcg.errored = true;
            return Maybe<IR::ASTValue>();
        }

        if (static_cast<IR::PointerType*>(opAsDeref->ptr.type().asRaw())->mut == false && method.thisMut) {
            ERR_MUT_ADDROF_NONMUT_OP(ast.dot, opAsDeref);
            fcg.errored = true;
            return Maybe<IR::ASTValue>();
        }

        return Maybe<IR::ASTValue>(IR::ASTValue(fcg.curBlock->add(std::make_unique<IR::Instrs::Addrof>(opAsDeref, method.thisMut)), op.ast));
    }() : op;

    if (!m_thisArg.has()) {
        return;
    }

    IR::ASTValue thisArg = m_thisArg.get();

    std::vector<IR::ASTValue> args { thisArg };

    CodeGen::ArgVisitor av (fcg, ast.args);
    args.insert(args.end(), av.ret.begin(), av.ret.end());

    std::vector<NNPtr<IR::Type>> &paramtys (method.fun->ty->paramtys);
    if (args.size() != paramtys.size()) {
        ERR_WRONG_NUM_ARGS(method.fun, ast, ast.oparn, args);
        ret = Maybe<IR::ASTValue>();
        fcg.errored = true;
        return;
    }

    // TODO: move this code somewhere else so that it does not have to be copied and pasted from visit_call_expr
    bool argserr = false;
    auto i = args.begin();
    auto j = paramtys.begin();
    for (; i != args.end() && j != paramtys.end(); ++i, ++j) {
        *i = (*j)->implCast(*cg.context, *fcg.fun, fcg.curBlock, *i);
        if (i->type() != *j) {
            ERR_INCORRECT_ARG(*i, *j);
            ret = Maybe<IR::ASTValue>();
            fcg.errored = true;
            argserr = true;
        }
    }

    if (argserr) {
        ret = Maybe<IR::ASTValue>();
        return;
    }

    ret = IR::ASTValue(fcg.curBlock->add(std::make_unique<IR::Instrs::Call>(method.fun, args)), ast);
}
