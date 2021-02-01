#include "codegenlocal.h"
#include "message/errmsgs.h"
#include "ast/ast.h"
#include "ir/instruction.h"
#include "ir/block.h"

CodeGen::Helpers::StmtCodeGen::StmtCodeGen(IR::Builder &ir_builder, Locals &locals, ExprCodeGen &expr_cg, TypeVisitor &type_visitor, PathVisitor &path_visitor):
    ir_builder(ir_builder),
    locals(locals),
    expr_cg(expr_cg),
    type_visitor(type_visitor),
    path_visitor(path_visitor) {}

void CodeGen::Helpers::StmtCodeGen::stmt(ASTNS::Stmt &ast) {
    ast.accept(*this);
}
void CodeGen::Helpers::StmtCodeGen::visit(ASTNS::ExprStmt &ast) {
    expr_cg->expr(*ast.expr);
}
void CodeGen::Helpers::StmtCodeGen::visit(ASTNS::VarStmt &ast) {
    for (std::unique_ptr<ASTNS::VarStmtItem> &item : ast.items)
        item->accept(*this);
}
void CodeGen::Helpers::StmtCodeGen::visit(ASTNS::RetStmt &ast) {
    Maybe<IR::ASTValue> m_v = ast.expr ? expr_cg->expr(*ast.expr) : Maybe<IR::ASTValue>(IR::ASTValue(ir_builder->context().get_void(), ast));
    if (!m_v.has())
        return;

    IR::ASTValue v = m_v.get();

    v = ir_builder->fun().ty->ret->impl_cast(ir_builder->context(), ir_builder->fun(), ir_builder->cur_block(), v);
    if (ir_builder->fun().ty->ret.as_raw() != &v.type()) {
        ERR_CONFLICT_RET_TY(v, ir_builder->fun());
        return;
    }

    ir_builder->cur_block()->add<IR::Instrs::Store>(IR::ASTValue(ir_builder->ret_reg(), ast), v, false);
    ir_builder->cur_block()->branch(std::make_unique<IR::Instrs::GotoBr>(ir_builder->exit_block()));
    ir_builder->cur_block() = ir_builder->fun().add_block("after_return");
}
