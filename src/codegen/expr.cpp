#include "codegenlocal.h"
#include "message/report_abort.h"
#include "message/errmsgs.h"
#include "ir/unit.h"
#include "ast/ast.h"
#include "ir/instruction.h"
#include "ir/function.h"
#include "ir/block.h"

Codegen::Helpers::ExprCodegen::ExprCodegen(IR::Builder &ir_builder, Helpers::Locals &locals, TypeVisitor &type_visitor, PathVisitor &path_visitor):
    ir_builder(ir_builder),
    locals(locals),
    stmt_cg(ir_builder, locals, *this, type_visitor, path_visitor),
    type_visitor(type_visitor),
    path_visitor(path_visitor) {}

Maybe<Located<NNPtr<IR::Value>>> Codegen::Helpers::ExprCodegen::expr(ASTNS::Expr &ast) {
    ret = Maybe<Located<NNPtr<IR::Value>>>();
    ast.ast_accept(*this);
    return ret;
}

void Codegen::Helpers::ExprCodegen::ast_visit(ASTNS::BinaryExpr &ast) {
    Maybe<Located<NNPtr<IR::Value>>> m_lhs = expr(*ast.lhs);
    Maybe<Located<NNPtr<IR::Value>>> m_rhs = expr(*ast.rhs);
    if (!m_lhs.has() || !m_rhs.has()) {
        ret = Maybe<Located<NNPtr<IR::Value>>>();
        return;
    }

    Located<NNPtr<IR::Value>> lhs = m_lhs.get(), rhs = m_rhs.get();
    ret = lhs.value->type().bin_op(ir_builder->context(), ir_builder->fun(), ir_builder->cur_block(), ast.op, lhs, rhs, ast);
}
void Codegen::Helpers::ExprCodegen::ast_visit(ASTNS::ShortCircuitExpr &ast) {
    Maybe<Located<NNPtr<IR::Value>>> m_lhs = expr(*ast.lhs);
    if (!m_lhs.has()) {
        ret = Maybe<Located<NNPtr<IR::Value>>>();
        return;
    }

    Located<NNPtr<IR::Value>> lhs = m_lhs.get();

    if (!dynamic_cast<IR::BoolType const *>(&lhs.value->type())) {
        ERR_LHS_UNSUPPORTED_OP(lhs, ast.op.span);
        ret = Maybe<Located<NNPtr<IR::Value>>>();
        return;
    }

    NNPtr<IR::Block> skip = ir_builder->fun().add_block("shortcircuit_skip");
    NNPtr<IR::Block> checkboth = ir_builder->fun().add_block("shortcircuit_checkboth");
    NNPtr<IR::Block> after = ir_builder->fun().add_block("shortcircuit_after");

    switch (ast.op.value) {
        case ASTNS::ShortCircuitOperator::DOUBLEPIPE:
            // jump to skip when true
            ir_builder->cur_block()->branch(std::make_unique<IR::Instrs::CondBr>(lhs, skip, checkboth));
            break;
        case ASTNS::ShortCircuitOperator::DOUBLEAMPER:
            // jump to skip when false
            ir_builder->cur_block()->branch(std::make_unique<IR::Instrs::CondBr>(lhs, checkboth, skip));
            break;
    }

    ir_builder->cur_block() = checkboth;

    Maybe<Located<NNPtr<IR::Value>>> m_rhs = expr(*ast.rhs);
    if (!m_rhs.has()) {
        ret = Maybe<Located<NNPtr<IR::Value>>>();
        return;
    }

    Located<NNPtr<IR::Value>> rhs = m_rhs.get();

    if (!dynamic_cast<IR::BoolType const *>(&rhs.value->type())) {
        ERR_CONFLICT_TYS_BINARY_OP(lhs, rhs, ast.op.span);
        ret = Maybe<Located<NNPtr<IR::Value>>>();
        return;
    }

    IR::Register &out = ir_builder->fun().add_register(lhs.value->type(), ast, false);

    ir_builder->cur_block()->add<IR::Instrs::Copy>(out, rhs);
    ir_builder->cur_block()->branch(std::make_unique<IR::Instrs::GotoBr>(after));

    ir_builder->cur_block() = skip;
    ir_builder->cur_block()->add<IR::Instrs::Copy>(out, lhs);
    ir_builder->cur_block()->branch(std::make_unique<IR::Instrs::GotoBr>(after));

    ir_builder->cur_block() = after;

    ret = Located<NNPtr<IR::Value>> { ast, out };
}

void Codegen::Helpers::ExprCodegen::ast_visit(ASTNS::UnaryExpr &ast) {
    Maybe<Located<NNPtr<IR::Value>>> m_oper = expr(*ast.expr);
    if (!m_oper.has()) {
        ret = Maybe<Located<NNPtr<IR::Value>>>();
        return;
    }

    Located<NNPtr<IR::Value>> oper = m_oper.get();

    ret = oper.value->type().unary_op(ir_builder->context(), ir_builder->fun(), ir_builder->cur_block(), ast.op, oper, ast);
}

void Codegen::Helpers::ExprCodegen::ast_visit(ASTNS::DerefExpr &ast) {
    Maybe<Located<NNPtr<IR::Value>>> m_oper = expr(*ast.expr);
    if (!m_oper.has()) {
        ret = Maybe<Located<NNPtr<IR::Value>>>();
        return;
    }

    Located<NNPtr<IR::Value>> oper = m_oper.get();

    IR::PointerType const *asptrty (dynamic_cast<IR::PointerType const *>(&oper.value->type()));
    if (!asptrty) {
        ERR_NO_DEREF(ast.op.span, oper);
        ret = Maybe<Located<NNPtr<IR::Value>>>();
        return;
    }

    ret = Located<NNPtr<IR::Value>> { ast, ir_builder->cur_block()->add<IR::Instrs::DerefPtr>(oper) };
}
void Codegen::Helpers::ExprCodegen::ast_visit(ASTNS::AddrofExpr &ast) {
    Maybe<Located<NNPtr<IR::Value>>> m_oper = expr(*ast.expr);
    if (!m_oper.has()) {
        ret = Maybe<Located<NNPtr<IR::Value>>>();
        return;
    }

    Located<NNPtr<IR::Value>> oper = m_oper.get();

    IR::Register *as_register = dynamic_cast<IR::Register *>(oper.value.as_raw());
    if (!as_register) {
        ERR_ADDROF_NOT_LVALUE(ast.op.span, oper);
        ret = Maybe<Located<NNPtr<IR::Value>>>();
        return;
    }

    if (!as_register->mut && ast.mut) {
        ERR_MUT_ADDROF_NONMUT_OP(ast.op.span, *as_register);
        ret = Maybe<Located<NNPtr<IR::Value>>>();
        return;
    }

    ret = Located<NNPtr<IR::Value>> { ast, ir_builder->cur_block()->add<IR::Instrs::Addrof>(*as_register, ast.mut) };
}

void Codegen::Helpers::ExprCodegen::ast_visit(ASTNS::CallExpr &ast) {
    Maybe<Located<NNPtr<IR::Value>>> m_fun = expr(*ast.callee);
    if (!m_fun.has()) {
        ret = Maybe<Located<NNPtr<IR::Value>>>();
        return;
    }

    Located<NNPtr<IR::Value>> fun = m_fun.get();

    IR::FunctionType const *fty = dynamic_cast<IR::FunctionType const *>(&fun.value->type());
    if (!fty) {
        ERR_CALL_NONCALLABLE(fun, ast.oparn.span);
        ret = Maybe<Located<NNPtr<IR::Value>>>();
        return;
    }

    Codegen::Helpers::ArgVisitor av (*this, ast.args);
    std::vector<Located<NNPtr<IR::Value>>> args (av.ret);

    if (args.size() != fty->paramtys.size()) {
        ERR_WRONG_NUM_ARGS(*static_cast<IR::Function const *>(fun.value.as_raw()), *ast.callee, ast.oparn.span, args);
        ret = Maybe<Located<NNPtr<IR::Value>>>();
        return;
    }

    bool argserr = false;
    auto i = args.begin();
    auto j = fty->paramtys.begin();
    for (; i != args.end() && j != fty->paramtys.end(); ++i, ++j) {
        *i = (*j)->impl_cast(ir_builder->context(), ir_builder->fun(), ir_builder->cur_block(), *i);
        if (&i->value->type() != &**j) {
            ERR_INCORRECT_ARG(*i, **j);
            ret = Maybe<Located<NNPtr<IR::Value>>>();
            argserr = true;
        }
    }

    if (argserr) {
        ret = Maybe<Located<NNPtr<IR::Value>>>();
        return;
    }

    ret = Located<NNPtr<IR::Value>> { ast, ir_builder->cur_block()->add<IR::Instrs::Call>(NNPtr<IR::Function const>(static_cast<IR::Function const *>(fun.value.as_raw())), args) };
}

void Codegen::Helpers::ExprCodegen::ast_visit(ASTNS::BoolLit &ast) {
    ret = Located<NNPtr<IR::Value>> { ast, ir_builder->context().get_const_bool(ast.val.value.val) };
}
void Codegen::Helpers::ExprCodegen::ast_visit(ASTNS::FloatLit &ast) {
    ret = Located<NNPtr<IR::Value>> { ast, ir_builder->context().get_const_float(ir_builder->context().get_generic_float_type(), ast.val.value.val) };
}
void Codegen::Helpers::ExprCodegen::ast_visit(ASTNS::IntLit &ast) {
    ret = Located<NNPtr<IR::Value>> { ast, ir_builder->context().get_const_int(ir_builder->context().get_generic_int_type(), ast.val.value.val) };
}
void Codegen::Helpers::ExprCodegen::ast_visit(ASTNS::CharLit &ast) {
    ret = Located<NNPtr<IR::Value>> { ast, ir_builder->context().get_const_char(ast.val.value.val) };
}
void Codegen::Helpers::ExprCodegen::ast_visit(ASTNS::StringLit &ast) {
    report_abort_noh("string literals are not supported yet");
}
void Codegen::Helpers::ExprCodegen::ast_visit(ASTNS::ThisExpr &ast) {
    Maybe<Local> m_loc = locals->get_local("this");
    if (m_loc.has()) {
        NNPtr<Local> local = m_loc.get();
        ret = Located<NNPtr<IR::Value>> { ast, *local->v };
    } else {
        ERR_NO_THIS(ast.tok.span);
        ret = Maybe<Located<NNPtr<IR::Value>>>();
    }
}
void Codegen::Helpers::ExprCodegen::ast_visit(ASTNS::IfExpr &ast) {
    Maybe<Located<NNPtr<IR::Value>>> m_cond = expr(*ast.cond);
    if (!m_cond.has()) {
        ret = Maybe<Located<NNPtr<IR::Value>>>();
        return;
    }
    Located<NNPtr<IR::Value>> cond = m_cond.get();

    if (!dynamic_cast<IR::BoolType const *>(&cond.value->type())) {
        ERR_COND_NOT_BOOL(cond);
        ret = Maybe<Located<NNPtr<IR::Value>>>();
        return;
    }

    NNPtr<IR::Block> trueb  = ir_builder->fun().add_block("if_true");
    Maybe<NNPtr<IR::Block>> falseb = ast.falses ? Maybe<NNPtr<IR::Block>>(ir_builder->fun().add_block("if_false")) : Maybe<NNPtr<IR::Block>>();
    NNPtr<IR::Block> afterb = ir_builder->fun().add_block("if_after");

    if (falseb.has())
        ir_builder->cur_block()->branch(std::make_unique<IR::Instrs::CondBr>(cond, trueb, falseb.get()));
    else
        ir_builder->cur_block()->branch(std::make_unique<IR::Instrs::CondBr>(cond, trueb, afterb));

    ir_builder->cur_block() = trueb;
    Maybe<Located<NNPtr<IR::Value>>> m_truev = expr(*ast.trues);
    if (!m_truev.has()) {
        ret = Maybe<Located<NNPtr<IR::Value>>>();
        return;
    }

    Located<NNPtr<IR::Value>> truev = m_truev.get();
    trueb = ir_builder->cur_block();

    IR::Register &ret_reg = ir_builder->fun().add_register(truev.value->type(), ast, false);
    trueb->add<IR::Instrs::Copy>(ret_reg, truev);
    trueb->branch(std::make_unique<IR::Instrs::GotoBr>(afterb));

    if (falseb.has()) {
        ir_builder->cur_block() = falseb.get();

        Maybe<Located<NNPtr<IR::Value>>> m_falsev = expr(*ast.falses);
        if (!m_falsev.has()) {
            ret = Maybe<Located<NNPtr<IR::Value>>>();
            return;
        }

        Located<NNPtr<IR::Value>> falsev = m_falsev.get();
        falseb = ir_builder->cur_block();

        falseb.get()->add<IR::Instrs::Copy>(ret_reg, truev);
        falseb.get()->branch(std::make_unique<IR::Instrs::GotoBr>(afterb));

        // try both implicit casts -- they are mostly asymmetrical so it should be fine
        truev = falsev.value->type().impl_cast(ir_builder->context(), ir_builder->fun(), ir_builder->cur_block(), truev);
        falsev = truev.value->type().impl_cast(ir_builder->context(), ir_builder->fun(), ir_builder->cur_block(), falsev);

        if (&truev.value->type() != &falsev.value->type()) {
            ERR_CONFL_TYS_IFEXPR(truev, falsev, ast.iftok.span, ast.elsetok.get().span);
            ret = Maybe<Located<NNPtr<IR::Value>>>();
            return;
        }
    } else {
        if (!dynamic_cast<IR::VoidType const *>(&truev.value->type())) {
            ERR_NO_ELSE_NOT_VOID(truev, ast.iftok.span);
            ret = Maybe<Located<NNPtr<IR::Value>>>();
            return;
        }
    }

    ir_builder->cur_block() = afterb;

    ret = Located<NNPtr<IR::Value>> { ast, ret_reg };
}
void Codegen::Helpers::ExprCodegen::ast_visit(ASTNS::WhileExpr &ast) {
    locals->inc_scope();

    IR::Block &loop_check_cond = ir_builder->fun().add_block("loop_checkcond");
    IR::Block &loop_body = ir_builder->fun().add_block("loop_body");
    IR::Block &loop_after = ir_builder->fun().add_block("loop_after");

    ir_builder->cur_block()->branch(std::make_unique<IR::Instrs::GotoBr>(loop_check_cond));
    ir_builder->cur_block() = loop_check_cond;

    Maybe<Located<NNPtr<IR::Value>>> m_cond = expr(*ast.cond);
    if (!m_cond.has()) {
        ret = Maybe<Located<NNPtr<IR::Value>>>();
        return;
    }

    Located<NNPtr<IR::Value>> cond = m_cond.get();

    if (!dynamic_cast<IR::BoolType const *>(&cond.value->type())) {
        ERR_COND_NOT_BOOL(cond);
        ret = Maybe<Located<NNPtr<IR::Value>>>();
        return;
    }
    ir_builder->cur_block()->branch(std::make_unique<IR::Instrs::CondBr>(cond, loop_body, loop_after));

    ir_builder->cur_block() = loop_body;
    expr(*ast.body);

    ir_builder->cur_block()->branch(std::make_unique<IR::Instrs::GotoBr>(loop_check_cond));

    locals->dec_scope();

    ir_builder->cur_block() = loop_after;

    ret = Located<NNPtr<IR::Value>> { ast, ir_builder->context().get_void() };
}

void Codegen::Helpers::ExprCodegen::ast_visit(ASTNS::AssignmentExpr &ast) {
    Maybe<Located<NNPtr<IR::Value>>> m_lhs = expr(*ast.target);
    Maybe<Located<NNPtr<IR::Value>>> m_rhs = expr(*ast.expr);

    if (!m_lhs.has() || !m_rhs.has()) {
        ret = Maybe<Located<NNPtr<IR::Value>>>();
        return;
    }

    Located<NNPtr<IR::Value>> lhs = m_lhs.get();
    Located<NNPtr<IR::Value>> rhs = m_rhs.get();

    IR::Register *target_reg = dynamic_cast<IR::Register *>(lhs.value.as_raw());

    if (!target_reg) {
        ERR_ASSIGN_INVALID_LHS(ast.equal.span, lhs);
        ret = Maybe<Located<NNPtr<IR::Value>>>();
        return;
    }

    if (!target_reg->mut) {
        ERR_ASSIGN_NOT_MUT(rhs, ast.equal.span, *target_reg);
        ret = Maybe<Located<NNPtr<IR::Value>>>();
        return;
    }

    IR::Type const &expect_type = target_reg->type();
    rhs = expect_type.impl_cast(ir_builder->context(), ir_builder->fun(), ir_builder->cur_block(), rhs);

    if (&expect_type != &rhs.value->type()) {
        ERR_ASSIGN_CONFLICT_TYS(lhs, rhs, ast.equal.span);
        ret = Maybe<Located<NNPtr<IR::Value>>>();
        return;
    }

    ir_builder->cur_block()->add<IR::Instrs::Copy>(*target_reg, rhs);
    ret = rhs;
}
void Codegen::Helpers::ExprCodegen::ast_visit(ASTNS::CastExpr &ast) {
    Maybe<Located<NNPtr<IR::Value>>> m_oper = expr(*ast.expr);
    if (!m_oper.has()) {
        ret = Maybe<Located<NNPtr<IR::Value>>>();
        return;
    }

    Located<NNPtr<IR::Value>> oper = m_oper.get();

    Maybe<IR::Type&> m_cast_to_ty = type_visitor->type(*ast.type);
    if (!m_cast_to_ty.has()) {
        return;
    }

    NNPtr<IR::Type> cast_to_ty = m_cast_to_ty.get();

    ret = cast_to_ty->cast_from(ir_builder->context(), ir_builder->fun(), ir_builder->cur_block(), oper, ast);
}

void Codegen::Helpers::ExprCodegen::ast_visit(ASTNS::Block &ast) {
    locals->inc_scope();

    for (auto stmt = ast.stmts.begin(); stmt != ast.stmts.end(); ++stmt) {
        stmt_cg.stmt(**stmt);
    }

    ASTNS::AST &void_ast = ast.stmts.size() ? *static_cast<ASTNS::AST*>(ast.stmts[ast.stmts.size() - 1].get()) : *static_cast<ASTNS::AST*>(&ast);

    if (ast.ret)
        ret = expr(*ast.ret);
    else
        ret = Located<NNPtr<IR::Value>> { void_ast, ir_builder->context().get_void() };

    if (!stmt_cg.success)
        ret = Maybe<Located<NNPtr<IR::Value>>>();

    locals->dec_scope();
}

void Codegen::Helpers::ExprCodegen::ast_visit(ASTNS::PathExpr &ast) {
    ret = path_visitor->resolve_value(*ast.path);
}
void Codegen::Helpers::ExprCodegen::ast_visit(ASTNS::FieldAccessExpr &ast) {
    Maybe<Located<NNPtr<IR::Value>>> m_op = expr(*ast.operand);
    if (!m_op.has()) {
        return;
    }

    Located<NNPtr<IR::Value>> op = m_op.get();

    std::string const &fieldName = ast.field.value.name;
    bool has = op.value->type().has_field(fieldName);
    if (!has) {
        ERR_NO_FIELD(op, ast.field.span);
        return;
    }

    int ind = op.value->type().get_field_index(fieldName);
    // TODO: do this
}
void Codegen::Helpers::ExprCodegen::ast_visit(ASTNS::MethodCallExpr &ast) {
    Maybe<Located<NNPtr<IR::Value>>> m_op = expr(*ast.operand);
    if (!m_op.has()) {
        return;
    }

    Located<NNPtr<IR::Value>> op = m_op.get();

    Maybe<IR::Type::Method const> m_method = op.value->type().get_method(ast.method.value.name);
    if (!m_method.has()) {
        ERR_NO_METHOD(op, ast.method.span);
        return;
    }

    IR::Type::Method method = m_method.get();

    Maybe<Located<NNPtr<IR::Value>>> m_this_arg;
    if (method.this_ptr) {
        IR::Register *as_register = dynamic_cast<IR::Register *>(op.value.as_raw());
        if (!as_register) {
            ERR_ADDROF_NOT_LVALUE(ast.dot.span, op);
            ret = Maybe<Located<NNPtr<IR::Value>>>();
            return;
        }

        if (!as_register->mut && method.this_mut) {
            ERR_MUT_ADDROF_NONMUT_OP(ast.method.span, *as_register);
            ret = Maybe<Located<NNPtr<IR::Value>>>();
            return;
        }

        m_this_arg = Located<NNPtr<IR::Value>> { op.span, ir_builder->cur_block()->add<IR::Instrs::Addrof>(*as_register, method.this_mut) };
    } else
        m_this_arg = op;

    Located<NNPtr<IR::Value>> this_arg = m_this_arg.get();

    std::vector<Located<NNPtr<IR::Value>>> args { this_arg };

    Codegen::Helpers::ArgVisitor av (*this, ast.args);
    args.insert(args.end(), av.ret.begin(), av.ret.end());

    std::vector<NNPtr<IR::Type const>> &paramtys (method.fun->ty->paramtys);
    if (args.size() != paramtys.size()) {
        ERR_WRONG_NUM_ARGS(*method.fun, ast, ast.oparn.span, args);
        ret = Maybe<Located<NNPtr<IR::Value>>>();
        return;
    }

    // TODO: move this code somewhere else so that it does not have to be copied and pasted from visiting call exprs
    bool argserr = false;
    auto i = args.begin();
    auto j = paramtys.begin();
    for (; i != args.end() && j != paramtys.end(); ++i, ++j) {
        *i = (*j)->impl_cast(ir_builder->context(), ir_builder->fun(), ir_builder->cur_block(), *i);
        if (&i->value->type() != j->as_raw()) {
            ERR_INCORRECT_ARG(*i, **j);
            ret = Maybe<Located<NNPtr<IR::Value>>>();
            argserr = true;
        }
    }

    if (argserr) {
        ret = Maybe<Located<NNPtr<IR::Value>>>();
        return;
    }

    ret = Located<NNPtr<IR::Value>> { ast, ir_builder->cur_block()->add<IR::Instrs::Call>(method.fun, args) };
}
