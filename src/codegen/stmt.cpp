#include "codegenlocal.h"
#include "message/errmsgs.h"

CodeGen::FunctionCodeGen::StmtCodeGen::StmtCodeGen(CodeGen &cg, FunctionCodeGen &fcg): cg(cg), fcg(fcg) {}

void CodeGen::FunctionCodeGen::StmtCodeGen::stmt(NNPtr<ASTNS::Stmt> ast) {
    ast->accept(this);
}
void CodeGen::FunctionCodeGen::StmtCodeGen::visitExprStmt(NNPtr<ASTNS::ExprStmt> ast) {
    fcg.exprCG.expr(ast->expr.get());
}
void CodeGen::FunctionCodeGen::StmtCodeGen::visitVarStmt(NNPtr<ASTNS::VarStmt> ast) {
    for (std::unique_ptr<ASTNS::VarStmtItem> &item : ast->items)
        item->accept(this);
}
void CodeGen::FunctionCodeGen::StmtCodeGen::visitRetStmt(NNPtr<ASTNS::RetStmt> ast) {
    Maybe<IR::ASTValue> m_v = ast->expr ? fcg.exprCG.expr(ast->expr.get()) : Maybe(IR::ASTValue(cg.context->getVoid(), ast));
    if (!m_v.has())
        return;

    IR::ASTValue v = m_v.get();

    v = fcg.ret->type()->implCast(*cg.context, *fcg.fun, fcg.curBlock, v);
    if (fcg.ret->type() != v.type()) {
        ERR_CONFLICT_RET_TY(v, fcg.fun);
        cg.errored = true;
        return;
    }

    fcg.curBlock->add(std::make_unique<IR::Instrs::Store>(IR::ASTValue(fcg.ret, ast), v, false));
    fcg.curBlock->branch(std::make_unique<IR::Instrs::GotoBr>(fcg.exitBlock));
    // fcg.curBlock = cg.context.blackHoleBlock.get(); TODO: fix
}
