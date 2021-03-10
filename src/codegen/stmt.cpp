#include "codegenlocal.h"
#include "ast/ast.h"
#include "ir/instruction.h"
#include "ir/block.h"

Codegen::Helpers::StmtCodegen::StmtCodegen(IR::Builder &ir_builder, Locals &locals, ExprCodegen &expr_cg, TypeVisitor &type_visitor, PathVisitor &path_visitor):
    success(true),
    ir_builder(ir_builder),
    locals(locals),
    expr_cg(expr_cg),
    type_visitor(type_visitor),
    path_visitor(path_visitor) {}

void Codegen::Helpers::StmtCodegen::stmt(ASTNS::Stmt &ast) {
    ast.ast_accept(*this);
}
void Codegen::Helpers::StmtCodegen::ast_visit(ASTNS::ExprStmt &ast) {
    Maybe<Located<NNPtr<IR::Value>>> res = expr_cg->expr(*ast.expr);
    if (!res.has())
        success = false;
}
void Codegen::Helpers::StmtCodegen::ast_visit(ASTNS::VarStmt &ast) {
    std::string const &varname = ast.name.value.name;
    Maybe<Codegen::Helpers::Local> var = locals->get_local(varname);
    if (var.has() && var.get().scopenum == locals->cur_scope) {
        ERR_REDECL_VAR(ast.name.span, *var.get().v);
        success = false;
        return;
    }

    Maybe<IR::Type&> m_var_type = type_visitor->type(*ast.type);
    if (!m_var_type.has()) {
        success = false;
        return;
    }

    NNPtr<IR::Type> var_type = m_var_type.get();

    IR::Register &reg = ir_builder->fun().add_register(*var_type, ast, ast.mut);

    if (ast.expr) {
        Maybe<Located<NNPtr<IR::Value>>> m_val = expr_cg->expr(*ast.expr);
        if (!m_val.has()) {
            success = false;
            return;
        }

        Located<NNPtr<IR::Value>> val = m_val.get();

        val = var_type->impl_cast(ir_builder->context(), ir_builder->fun(), ir_builder->cur_block(), val);
        if (&val.value->type() != var_type.as_raw()) {
            ERR_CONFLICT_VAR_INIT_TY(ast.equal.get().span, ast.name.span, *ast.type, val, *var_type);
            success = false;
            return;
        }
        ir_builder->cur_block()->add<IR::Instrs::Copy>(reg, val);
    } else if (!ast.mut) // no initializer, not mutable
        WARN_IMMUT_NOINIT(ast);

    locals->add_local(varname, reg);
}
void Codegen::Helpers::StmtCodegen::ast_visit(ASTNS::RetStmt &ast) {
    Maybe<Located<NNPtr<IR::Value>>> m_v = ast.expr ? expr_cg->expr(*ast.expr) : Maybe<Located<NNPtr<IR::Value>>>(Located<NNPtr<IR::Value>> { ast, ir_builder->context().get_void() });
    if (!m_v.has()) {
        success = false;
        return;
    }

    Located<NNPtr<IR::Value>> v = m_v.get();

    v = ir_builder->fun().ty->ret->impl_cast(ir_builder->context(), ir_builder->fun(), ir_builder->cur_block(), v);
    if (ir_builder->fun().ty->ret.as_raw() != &v.value->type()) {
        ERR_CONFLICT_RET_TY(v, ir_builder->fun());
        success = false;
        return;
    }

    ir_builder->cur_block()->add<IR::Instrs::Copy>(*ir_builder->fun().ret_reg, v);
    ir_builder->cur_block()->branch(std::make_unique<IR::Instrs::GotoBr>(ir_builder->exit_block()));
    ir_builder->cur_block() = ir_builder->fun().add_block("after_return");
}
