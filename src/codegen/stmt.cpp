#include "codegenlocal.h"
#include "message/errmsgs.h"
#include "ast/ast.h"
#include "ir/instruction.h"
#include "ir/block.h"

CodeGen::Helpers::StmtCodeGen::StmtCodeGen(IR::Builder &builder, ExprCodeGen &expr_cg):
    builder(builder),
    expr_cg(expr_cg) {}

void CodeGen::Helpers::StmtCodeGen::stmt(ASTNS::Stmt &ast) {
    ast.accept(*this);
}
void CodeGen::Helpers::StmtCodeGen::visit(ASTNS::ExprStmt &ast) {
    expr_cg.expr(*ast.expr);
}
void CodeGen::Helpers::StmtCodeGen::visit(ASTNS::VarStmt &ast) {
    for (std::unique_ptr<ASTNS::VarStmtItem> &item : ast.items)
        item->accept(*this);
}
void CodeGen::Helpers::StmtCodeGen::visit(ASTNS::RetStmt &ast) {
    Maybe<IR::ASTValue> m_v = ast.expr ? expr_cg.expr(*ast.expr) : Maybe<IR::ASTValue>(IR::ASTValue(builder.context().get_void(), ast));
    if (!m_v.has())
        return;

    IR::ASTValue v = m_v.get();

    v = builder.fun().ty->ret->impl_cast(builder.context(), builder.fun(), builder.cur_block(), v);
    if (builder.fun().ty->ret.as_raw() != &v.type()) {
        ERR_CONFLICT_RET_TY(v, builder.fun());
        return;
    }

    builder.cur_block()->add<IR::Instrs::Store>(IR::ASTValue(builder.ret_reg(), ast), v, false);
    builder.cur_block()->branch(std::make_unique<IR::Instrs::GotoBr>(builder.exit_block()));
    builder.cur_block() = builder.fun().add_block("after_return");
}
