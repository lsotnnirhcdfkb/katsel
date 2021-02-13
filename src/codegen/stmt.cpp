#include "codegenlocal.h"
#include "message/errmsgs.h"
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
    ast.accept(*this);
}
void Codegen::Helpers::StmtCodegen::visit(ASTNS::ExprStmt &ast) {
    Maybe<Located<NNPtr<IR::Value>>> res = expr_cg->expr(*ast.expr);
    if (!res.has())
        success = false;
}
void Codegen::Helpers::StmtCodegen::visit(ASTNS::VarStmt &ast) {
    for (std::unique_ptr<ASTNS::VarStmtItem> &item : ast.items)
        item->accept(*this);
}
void Codegen::Helpers::StmtCodegen::visit(ASTNS::RetStmt &ast) {
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
