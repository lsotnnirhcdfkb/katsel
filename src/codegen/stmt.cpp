#include "codegenlocal.h"
#include "message/errmsgs.h"
#include "ast/ast.h"
#include "ir/instruction.h"

CodeGen::FunctionCodeGen::StmtCodeGen::StmtCodeGen(CodeGen &cg, FunctionCodeGen &fcg): cg(cg), fcg(fcg) {}

void CodeGen::FunctionCodeGen::StmtCodeGen::stmt(NNPtr<ASTNS::Stmt> ast) {
    ast->accept(*this);
}
void CodeGen::FunctionCodeGen::StmtCodeGen::visit(ASTNS::ExprStmt &ast) {
    fcg.expr_cg.expr(ast.expr.get());
}
void CodeGen::FunctionCodeGen::StmtCodeGen::visit(ASTNS::VarStmt &ast) {
    for (std::unique_ptr<ASTNS::VarStmtItem> &item : ast.items)
        item->accept(*this);
}
void CodeGen::FunctionCodeGen::StmtCodeGen::visit(ASTNS::RetStmt &ast) {
    Maybe<IR::ASTValue> m_v = ast.expr ? fcg.expr_cg.expr(ast.expr.get()) : Maybe<IR::ASTValue>(IR::ASTValue(cg.context->get_void(), ast));
    if (!m_v.has())
        return;

    IR::ASTValue v = m_v.get();

    v = fcg.ret->type()->impl_cast(*cg.context, *fcg.fun, fcg.cur_block, v);
    if (fcg.ret->type() != v.type()) {
        ERR_CONFLICT_RET_TY(v, fcg.fun);
        cg.errored = true;
        return;
    }

    fcg.cur_block->add(std::make_unique<IR::Instrs::Store>(IR::ASTValue(fcg.ret, ast), v, false));
    fcg.cur_block->branch(std::make_unique<IR::Instrs::GotoBr>(fcg.exit_block));
    // fcg.cur_block = cg.context.black_hole_block.get(); TODO: fix
}
