#include "codegenlocal.h"
#include "message/errmsgs.h"
#include "ast/ast.h"
#include "ir/instruction.h"
#include "ir/block.h"

CodeGen::Helpers::StmtCodeGen::StmtCodeGen(CodeGen::Context &context, IR::Function &fun, IR::Block &exit_block, IR::Instrs::Register &ret_reg, NNPtr<IR::Block> &cur_block, ExprCodeGen &expr_cg):
    context(context),
    fun(fun),
    exit_block(exit_block),
    ret_reg(ret_reg),
    cur_block(cur_block),
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
    Maybe<IR::ASTValue> m_v = ast.expr ? expr_cg.expr(*ast.expr) : Maybe<IR::ASTValue>(IR::ASTValue(context.get_void(), ast));
    if (!m_v.has())
        return;

    IR::ASTValue v = m_v.get();

    v = fun.ty->ret->impl_cast(context, fun, cur_block, v);
    if (fun.ty->ret.as_raw() != &v.type()) {
        ERR_CONFLICT_RET_TY(v, fun);
        return;
    }

    cur_block->add<IR::Instrs::Store>(IR::ASTValue(ret_reg, ast), v, false);
    cur_block->branch(std::make_unique<IR::Instrs::GotoBr>(exit_block));
    cur_block = fun.add_block("after_return");
}
