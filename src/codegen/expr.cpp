#include "codegenlocal.h"
#include "message/internal.h"
#include "message/report_abort.h"
#include "message/errmsgs.h"
#include "ir/unit.h"
#include "ast/ast.h"
#include "ir/instruction.h"
#include "ir/function.h"
#include "ir/block.h"

CodeGen::FunctionCodeGen::ExprCodeGen::ExprCodeGen(CodeGen &cg, FunctionCodeGen &fcg): cg(cg), fcg(fcg) {}

Maybe<IR::ASTValue> CodeGen::FunctionCodeGen::ExprCodeGen::expr(ASTNS::Expr &ast) {
    ret = Maybe<IR::ASTValue>();
    ast.accept(*this);
    return ret;
}

void CodeGen::FunctionCodeGen::ExprCodeGen::visit(ASTNS::BinaryExpr &ast) {
    Maybe<IR::ASTValue> m_lhs = expr(*ast.lhs);
    Maybe<IR::ASTValue> m_rhs = expr(*ast.rhs);
    if (!m_lhs.has() || !m_rhs.has()) {
        ret = Maybe<IR::ASTValue>();
        return;
    }

    IR::ASTValue lhs = m_lhs.get(), rhs = m_rhs.get();

    IR::Type::BinaryOperator oper;
    switch (ast.op.index()) {
        case Token::index_of<Tokens::BangEqual>: oper = IR::Type::BinaryOperator::bangequal; break;
        case Token::index_of<Tokens::DoubleEqual>: oper = IR::Type::BinaryOperator::doubleequal; break;
        case Token::index_of<Tokens::Less>: oper = IR::Type::BinaryOperator::less; break;
        case Token::index_of<Tokens::Greater>: oper = IR::Type::BinaryOperator::greater; break;
        case Token::index_of<Tokens::LessEqual>: oper = IR::Type::BinaryOperator::lessequal; break;
        case Token::index_of<Tokens::GreaterEqual>: oper = IR::Type::BinaryOperator::greaterequal; break;
        case Token::index_of<Tokens::Caret>: oper = IR::Type::BinaryOperator::caret; break;
        case Token::index_of<Tokens::Pipe>: oper = IR::Type::BinaryOperator::pipe; break;
        case Token::index_of<Tokens::Amper>: oper = IR::Type::BinaryOperator::amper; break;
        case Token::index_of<Tokens::DoubleGreater>: oper = IR::Type::BinaryOperator::doublegreater; break;
        case Token::index_of<Tokens::DoubleLess>: oper = IR::Type::BinaryOperator::doubleless; break;
        case Token::index_of<Tokens::Plus>: oper = IR::Type::BinaryOperator::plus; break;
        case Token::index_of<Tokens::Minus>: oper = IR::Type::BinaryOperator::minus; break;
        case Token::index_of<Tokens::Star>: oper = IR::Type::BinaryOperator::star; break;
        case Token::index_of<Tokens::Slash>: oper = IR::Type::BinaryOperator::slash; break;
        case Token::index_of<Tokens::Percent>: oper = IR::Type::BinaryOperator::percent; break;
        default: invalid_tok("binary operator", ast.op);
    }
    ret = lhs.type().bin_op(*cg.context, *fcg.fun, fcg.cur_block, oper, lhs, rhs, ast.op, ast);
    if (!ret.has())
        fcg.errored = true;
}
void CodeGen::FunctionCodeGen::ExprCodeGen::visit(ASTNS::ShortCircuitExpr &ast) {
    Maybe<IR::ASTValue> m_lhs = expr(*ast.lhs);
    if (!m_lhs.has()) {
        ret = Maybe<IR::ASTValue>();
        return;
    }

    IR::ASTValue lhs = m_lhs.get();

    if (!dynamic_cast<IR::BoolType const *>(&lhs.type())) {
        ERR_LHS_UNSUPPORTED_OP(lhs, ast.op);
        ret = Maybe<IR::ASTValue>();
        fcg.errored = true;
        return;
    }

    NNPtr<IR::Block> skip = fcg.fun->add_block("shortcircuit_skip");
    NNPtr<IR::Block> checkboth = fcg.fun->add_block("shortcircuit_checkboth");
    NNPtr<IR::Block> after = fcg.fun->add_block("shortcircuit_after");

    bool value_if_skipped;
    if (ast.op.is<Tokens::DoublePipe>()) {
        // jump to skip when true
        fcg.cur_block->branch(std::make_unique<IR::Instrs::CondBr>(lhs, skip, checkboth));
        value_if_skipped = true;
    } else if (ast.op.is<Tokens::DoubleAmper>()) {
        // jump to skip when false
        fcg.cur_block->branch(std::make_unique<IR::Instrs::CondBr>(lhs, checkboth, skip));
        value_if_skipped = false;
    } else
        invalid_tok("short circuiting operator", ast.op);

    fcg.cur_block = checkboth;

    Maybe<IR::ASTValue> m_rhs = expr(*ast.rhs);
    if (!m_rhs.has()) {
        ret = Maybe<IR::ASTValue>();
        return;
    }

    IR::ASTValue rhs = m_rhs.get();

    if (!dynamic_cast<IR::BoolType const *>(&rhs.type())) {
        ERR_CONFLICT_TYS_BINARY_OP(lhs, rhs, ast.op);
        ret = Maybe<IR::ASTValue>();
        fcg.errored = true;
        return;
    }
    fcg.cur_block->branch(std::make_unique<IR::Instrs::GotoBr>(after));

    fcg.cur_block = skip;
    fcg.cur_block->branch(std::make_unique<IR::Instrs::GotoBr>(after));

    fcg.cur_block = after;
    ret = IR::ASTValue(fcg.cur_block->add<IR::Instrs::Phi>(std::vector<std::pair<NNPtr<IR::Block const>, IR::ASTValue>>{std::make_pair(checkboth, rhs), std::make_pair(skip, IR::ASTValue(cg.context->get_const_bool(value_if_skipped), ast))}), ast);
}

void CodeGen::FunctionCodeGen::ExprCodeGen::visit(ASTNS::UnaryExpr &ast) {
    Maybe<IR::ASTValue> m_oper = expr(*ast.expr);
    if (!m_oper.has()) {
        ret = Maybe<IR::ASTValue>();
        return;
    }

    IR::ASTValue oper = m_oper.get();

    IR::Type::UnaryOperator opor;

    switch (ast.op.index()) {
        case Token::index_of<Tokens::Tilde>:
            opor = IR::Type::UnaryOperator::tilde; break;
        case Token::index_of<Tokens::Minus>:
            opor = IR::Type::UnaryOperator::minus; break;
        case Token::index_of<Tokens::Bang>:
            opor = IR::Type::UnaryOperator::bang; break;
        default:
            invalid_tok("unary operator", ast.op);
    }

    ret = oper.type().unary_op(*cg.context, *fcg.fun, fcg.cur_block, opor, oper, ast.op, ast);
    if (!ret.has())
        fcg.errored = true;
}

void CodeGen::FunctionCodeGen::ExprCodeGen::visit(ASTNS::DerefExpr &ast) {
    Maybe<IR::ASTValue> m_oper = expr(*ast.expr);
    if (!m_oper.has()) {
        ret = Maybe<IR::ASTValue>();
        return;
    }

    IR::ASTValue oper = m_oper.get();

    IR::PointerType const *asptrty (dynamic_cast<IR::PointerType const *>(&oper.type()));
    if (!asptrty) {
        ERR_NO_DEREF(ast.op, oper);
        ret = Maybe<IR::ASTValue>();
        fcg.errored = true;
        return;
    }

    ret = IR::ASTValue(fcg.cur_block->add<IR::Instrs::DerefPtr>(oper), ast);
}
void CodeGen::FunctionCodeGen::ExprCodeGen::visit(ASTNS::AddrofExpr &ast) {
    Maybe<IR::ASTValue> m_oper = expr(*ast.expr);
    if (!m_oper.has()) {
        ret = Maybe<IR::ASTValue>();
        return;
    }

    IR::ASTValue oper = m_oper.get();

    IR::Instrs::DerefPtr const *as_deref = dynamic_cast<IR::Instrs::DerefPtr const *>(oper.val.as_raw());
    if (!as_deref) {
        ERR_ADDROF_NOT_LVALUE(ast.op, oper);
        ret = Maybe<IR::ASTValue>();
        fcg.errored = true;
        return;
    }

    if (static_cast<IR::PointerType const *>(&as_deref->ptr.type())->mut == false && ast.mut) {
        ERR_MUT_ADDROF_NONMUT_OP(ast.op, *as_deref);
        ret = Maybe<IR::ASTValue>();
        fcg.errored = true;
        return;
    }

    ret = IR::ASTValue(fcg.cur_block->add<IR::Instrs::Addrof>(as_deref, ast.mut), ast);
}

void CodeGen::FunctionCodeGen::ExprCodeGen::visit(ASTNS::CallExpr &ast) {
    Maybe<IR::ASTValue> m_fun = expr(*ast.callee);
    if (!m_fun.has()) {
        ret = Maybe<IR::ASTValue>();
        return;
    }

    IR::ASTValue fun = m_fun.get();

    IR::FunctionType const *fty = dynamic_cast<IR::FunctionType const *>(&fun.type());
    if (!fty) {
        ERR_CALL_NONCALLABLE(fun, ast.oparn);
        ret = Maybe<IR::ASTValue>();
        fcg.errored = true;
        return;
    }

    CodeGen::ArgVisitor av (fcg, ast.args);
    std::vector<IR::ASTValue> args (av.ret);

    if (args.size() != fty->paramtys.size()) {
        ERR_WRONG_NUM_ARGS(*static_cast<IR::Function const *>(fun.val.as_raw()), *ast.callee, ast.oparn, args);
        ret = Maybe<IR::ASTValue>();
        fcg.errored = true;
        return;
    }

    bool argserr = false;
    auto i = args.begin();
    auto j = fty->paramtys.begin();
    for (; i != args.end() && j != fty->paramtys.end(); ++i, ++j) {
        *i = (*j)->impl_cast(*cg.context, *fcg.fun, fcg.cur_block, *i);
        if (&i->type() != &**j) {
            ERR_INCORRECT_ARG(*i, **j);
            ret = Maybe<IR::ASTValue>();
            fcg.errored = true;
            argserr = true;
        }
    }

    if (argserr) {
        ret = Maybe<IR::ASTValue>();
        return;
    }

    ret = IR::ASTValue(fcg.cur_block->add<IR::Instrs::Call>(NNPtr<IR::Function const>(static_cast<IR::Function const *>(fun.val.as_raw())), args), ast);
}

void CodeGen::FunctionCodeGen::ExprCodeGen::visit(ASTNS::PrimaryExpr &ast) {
    switch (ast.value.index()) {
        case Token::index_of<Tokens::BoolLit>:
            ret = IR::ASTValue(cg.context->get_const_bool(ast.value.as<Tokens::BoolLit>().val), ast);
            return;

        case Token::index_of<Tokens::FloatLit>:
            ret = IR::ASTValue(cg.context->get_const_float(cg.context->get_generic_float_type(), ast.value.as<Tokens::FloatLit>().val), ast);
            return;

        case Token::index_of<Tokens::IntLit>:
            ret = IR::ASTValue(cg.context->get_const_int(cg.context->get_generic_int_type(), ast.value.as<Tokens::IntLit>().val), ast);
            return;

        case Token::index_of<Tokens::CharLit>:
            ret = IR::ASTValue(cg.context->get_const_char(ast.value.as<Tokens::CharLit>().val), ast);
            return;

        case Token::index_of<Tokens::StringLit>:
            report_abort_noh("string literals are not supported yet");

        case Token::index_of<Tokens::This>: {
                Maybe<Local&> m_loc = fcg.get_local("this");
                if (m_loc.has()) {
                    NNPtr<Local> local = m_loc.get();
                    ret = IR::ASTValue(fcg.cur_block->add<IR::Instrs::DerefPtr>(IR::ASTValue(*local->v, ast)), ast);
                } else {
                    ERR_NO_THIS(ast.value);
                    fcg.errored = true;
                    ret = Maybe<IR::ASTValue>();
                }
                return;
            }

        default:
            invalid_tok("primary token", ast.value);
    }
}
void CodeGen::FunctionCodeGen::ExprCodeGen::visit(ASTNS::IfExpr &ast) {
    Maybe<IR::ASTValue> m_cond = expr(*ast.cond);
    if (!m_cond.has()) {
        ret = Maybe<IR::ASTValue>();
        return;
    }
    IR::ASTValue cond = m_cond.get();

    if (!dynamic_cast<IR::BoolType const *>(&cond.type())) {
        ERR_COND_NOT_BOOL(cond);
        ret = Maybe<IR::ASTValue>();
        fcg.errored = true;
        return;
    }

    NNPtr<IR::Block> trueb  = fcg.fun->add_block("if_true");
    Maybe<NNPtr<IR::Block>> falseb = ast.falses ? Maybe<NNPtr<IR::Block>>(fcg.fun->add_block("if_false")) : Maybe<NNPtr<IR::Block>>();
    NNPtr<IR::Block> afterb = fcg.fun->add_block("if_after");

    if (falseb.has())
        fcg.cur_block->branch(std::make_unique<IR::Instrs::CondBr>(cond, trueb, falseb.get()));
    else
        fcg.cur_block->branch(std::make_unique<IR::Instrs::CondBr>(cond, trueb, afterb));

    fcg.cur_block = trueb;
    Maybe<IR::ASTValue> m_truev = expr(*ast.trues);
    if (!m_truev.has()) {
        ret = Maybe<IR::ASTValue>();
        return;
    }

    IR::ASTValue truev = m_truev.get();

    fcg.cur_block->branch(std::make_unique<IR::Instrs::GotoBr>(afterb));
    trueb = fcg.cur_block;

    Maybe<IR::ASTValue> falsev;
    if (falseb.has()) {
        fcg.cur_block = falseb.get();

        Maybe<IR::ASTValue> m_falsev = expr(*ast.falses);
        if (!m_falsev.has()) {
            ret = Maybe<IR::ASTValue>();
            return;
        }

        falsev = m_falsev;

        fcg.cur_block->branch(std::make_unique<IR::Instrs::GotoBr>(afterb));
        falseb = fcg.cur_block;

        // try both implicit casts -- they are mostly asymmetrical so it should be fine
        truev = falsev.get().type().impl_cast(*cg.context, *fcg.fun, fcg.cur_block, truev);
        falsev = truev.type().impl_cast(*cg.context, *fcg.fun, fcg.cur_block, falsev.get());

        if (&truev.type() != &falsev.get().type()) {
            ERR_CONFL_TYS_IFEXPR(truev, falsev.get(), ast.iftok, ast.elsetok);
            ret = Maybe<IR::ASTValue>();
            fcg.errored = true;
            return;
        }
    } else {
        if (!dynamic_cast<IR::VoidType const *>(&truev.type())) {
            ERR_NO_ELSE_NOT_VOID(truev, ast.iftok);
            ret = Maybe<IR::ASTValue>();
            fcg.errored = true;
            return;
        }
    }

    fcg.cur_block = afterb;

    if (falseb.has()) {
        ret = IR::ASTValue(afterb->add<IR::Instrs::Phi>(std::vector<std::pair<NNPtr<IR::Block const>, IR::ASTValue>>({ std::make_pair(trueb, truev), std::make_pair(falseb.get(), falsev.get()) })), ast);
    } else
        ret = IR::ASTValue(*truev.val, ast);

}
void CodeGen::FunctionCodeGen::ExprCodeGen::visit(ASTNS::WhileExpr &ast) {
    fcg.inc_scope();

    NNPtr<IR::Block> loop_check_cond = fcg.fun->add_block("loop_checkcond");
    NNPtr<IR::Block> loop_body = fcg.fun->add_block("loop_body");
    NNPtr<IR::Block> loop_after = fcg.fun->add_block("loop_after");

    fcg.cur_block->branch(std::make_unique<IR::Instrs::GotoBr>(loop_check_cond));
    fcg.cur_block = loop_check_cond;

    Maybe<IR::ASTValue> m_cond = expr(*ast.cond);
    if (!m_cond.has()) {
        ret = Maybe<IR::ASTValue>();
        return;
    }

    IR::ASTValue cond = m_cond.get();

    if (!dynamic_cast<IR::BoolType const *>(&cond.type())) {
        ERR_COND_NOT_BOOL(cond);
        ret = Maybe<IR::ASTValue>();
        fcg.errored = true;
        return;
    }
    fcg.cur_block->branch(std::make_unique<IR::Instrs::CondBr>(cond, loop_body, loop_after));

    fcg.cur_block = loop_body;
    expr(*ast.body);

    fcg.cur_block->branch(std::make_unique<IR::Instrs::GotoBr>(loop_check_cond));

    fcg.dec_scope();

    fcg.cur_block = loop_after;

    ret = IR::ASTValue(cg.context->get_void(), ast);
}

void CodeGen::FunctionCodeGen::ExprCodeGen::visit(ASTNS::AssignmentExpr &ast) {
    Maybe<IR::ASTValue> m_lhs = expr(*ast.target);
    Maybe<IR::ASTValue> m_rhs = expr(*ast.expr);

    if (!m_lhs.has() || !m_rhs.has()) {
        ret = Maybe<IR::ASTValue>();
        return;
    }

    IR::ASTValue lhs = m_lhs.get();
    IR::ASTValue rhs = m_rhs.get();

    IR::Instrs::DerefPtr const *target_deref = dynamic_cast<IR::Instrs::DerefPtr const *>(lhs.val.as_raw());

    if (!target_deref) {
        ERR_ASSIGN_INVALID_LHS(ast.equal, lhs);
        ret = Maybe<IR::ASTValue>();
        fcg.errored = true;
        return;
    }

    if (!static_cast<IR::PointerType const *>(&target_deref->ptr.type())->mut) {
        ERR_ASSIGN_NOT_MUT(lhs, ast.equal, *target_deref);
        ret = Maybe<IR::ASTValue>();
        fcg.errored = true;
        return;
    }

    NNPtr<IR::Type const> expect_type = target_deref->type();
    rhs = expect_type->impl_cast(*cg.context, *fcg.fun, fcg.cur_block, rhs);
    if (expect_type.as_raw() != &rhs.type()) {
        ERR_ASSIGN_CONFLICT_TYS(lhs, rhs, ast.equal);
        ret = Maybe<IR::ASTValue>();
        fcg.errored = true;
        return;
    }

    fcg.cur_block->add<IR::Instrs::Store>(target_deref->ptr, rhs, false);

    ret = rhs;
}
void CodeGen::FunctionCodeGen::ExprCodeGen::visit(ASTNS::CastExpr &ast) {
    Maybe<IR::ASTValue> m_oper = expr(*ast.expr);
    if (!m_oper.has()) {
        ret = Maybe<IR::ASTValue>();
        return;
    }

    IR::ASTValue oper = m_oper.get();

    Maybe<IR::Type&> m_cast_to_ty = cg.type_visitor->type(*ast.type, fcg.this_type);
    if (!m_cast_to_ty.has()) {
        fcg.errored = true;
        return;
    }

    NNPtr<IR::Type> cast_to_ty = m_cast_to_ty.get();

    ret = cast_to_ty->cast_from(*cg.context, *fcg.fun, fcg.cur_block, oper, ast);

    if (!ret.has())
        fcg.errored = true;
}

void CodeGen::FunctionCodeGen::ExprCodeGen::visit(ASTNS::Block &ast) {
    fcg.inc_scope();

    Maybe<IR::ASTValue> block_ret;

    for (auto stmt = ast.stmts.begin(); stmt != ast.stmts.end(); ++stmt) {
        if (ASTNS::ExprStmt *exprstmt = dynamic_cast<ASTNS::ExprStmt*>(stmt->get())) {
            bool last = stmt + 1 == ast.stmts.end();

            if (last && !exprstmt->suppress) {
                // if the stmt is the last stmt of the block, and is not suppressed
                block_ret = expr(*exprstmt->expr);
                if (!block_ret.has()) {
                    fcg.errored = true;
                    return;
                }
            } else {
                // if the stmt does not count as a return value
                // (ie it is not the last stmt, or it is the last stmt and is suppressed)
                expr(*exprstmt->expr);

                if (!last && exprstmt->suppress) {
                    ERR_NO_SUPPRESS(exprstmt->dot.get());
                    fcg.errored = true;
                }
            }
        } else {
            fcg.stmt_cg.stmt(**stmt);
        }
    }

    ASTNS::AST &void_ast = ast.stmts.size() ? *static_cast<ASTNS::AST*>(ast.stmts[ast.stmts.size() - 1].get()) : *static_cast<ASTNS::AST*>(&ast);

    ret = block_ret.has() ? block_ret.get() : IR::ASTValue(cg.context->get_void(), void_ast);

    fcg.dec_scope();
}

void CodeGen::FunctionCodeGen::ExprCodeGen::visit(ASTNS::PathExpr &ast) {
    ret = cg.path_visitor->resolve_value(*ast.path, fcg);
    if (!ret.has())
        fcg.errored = true;
}
void CodeGen::FunctionCodeGen::ExprCodeGen::visit(ASTNS::FieldAccessExpr &ast) {
    Maybe<IR::ASTValue> m_op = expr(*ast.operand);
    if (!m_op.has()) {
        fcg.errored = true;
        return;
    }

    IR::ASTValue op = m_op.get();

    std::string fieldName = ast.field.as<Tokens::Identifier>().name;
    bool has = op.type().has_field(fieldName);
    if (!has) {
        ERR_NO_FIELD(op, ast.field);
        fcg.errored = true;
        return;
    }

    int ind = op.type().get_field_index(fieldName);
    // TODO: do this
}
void CodeGen::FunctionCodeGen::ExprCodeGen::visit(ASTNS::MethodCallExpr &ast) {
    Maybe<IR::ASTValue> m_op = expr(*ast.operand);
    if (!m_op.has()) {
        fcg.errored = true;
        return;
    }

    IR::ASTValue op = m_op.get();

    Maybe<IR::Type::Method const> m_method = op.type().get_method(ast.method.as<Tokens::Identifier>().name);
    if (!m_method.has()) {
        ERR_NO_METHOD(op, ast.method);
        fcg.errored = true;
        return;
    }

    IR::Type::Method method = m_method.get();

    Maybe<IR::ASTValue> m_this_arg;
    if (method.this_ptr) {
        IR::Instrs::DerefPtr const *op_as_deref = dynamic_cast<IR::Instrs::DerefPtr const *>(op.val.as_raw());
        if (!op_as_deref) {
            ERR_ADDROF_NOT_LVALUE(ast.dot, op);
            fcg.errored = true;
            ret = Maybe<IR::ASTValue>();
            return;
        }

        if (static_cast<IR::PointerType const *>(&op_as_deref->ptr.type())->mut == false && method.this_mut) {
            ERR_MUT_ADDROF_NONMUT_OP(ast.dot, *op_as_deref);
            fcg.errored = true;
            ret = Maybe<IR::ASTValue>();
            return;
        }

        m_this_arg = Maybe<IR::ASTValue>(IR::ASTValue(fcg.cur_block->add<IR::Instrs::Addrof>(op_as_deref, method.this_mut), *op.ast));
    } else
        m_this_arg = op;

    IR::ASTValue this_arg = m_this_arg.get();

    std::vector<IR::ASTValue> args { this_arg };

    CodeGen::ArgVisitor av (fcg, ast.args);
    args.insert(args.end(), av.ret.begin(), av.ret.end());

    std::vector<NNPtr<IR::Type const>> &paramtys (method.fun->ty->paramtys);
    if (args.size() != paramtys.size()) {
        ERR_WRONG_NUM_ARGS(*method.fun, ast, ast.oparn, args);
        ret = Maybe<IR::ASTValue>();
        fcg.errored = true;
        return;
    }

    // TODO: move this code somewhere else so that it does not have to be copied and pasted from visiting call exprs
    bool argserr = false;
    auto i = args.begin();
    auto j = paramtys.begin();
    for (; i != args.end() && j != paramtys.end(); ++i, ++j) {
        *i = (*j)->impl_cast(*cg.context, *fcg.fun, fcg.cur_block, *i);
        if (&i->type() != j->as_raw()) {
            ERR_INCORRECT_ARG(*i, **j);
            ret = Maybe<IR::ASTValue>();
            fcg.errored = true;
            argserr = true;
        }
    }

    if (argserr) {
        ret = Maybe<IR::ASTValue>();
        return;
    }

    ret = IR::ASTValue(fcg.cur_block->add<IR::Instrs::Call>(method.fun, args), ast);
}
