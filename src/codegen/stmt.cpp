#include "codegenlocal.h"
#include "message/errmsgs.h"

CodeGen::FunctionCodeGen::StmtCodeGen::StmtCodeGen(CodeGen &cg, FunctionCodeGen &fcg): cg(cg), fcg(fcg) {}

void CodeGen::FunctionCodeGen::StmtCodeGen::stmt(ASTNS::Stmt *ast) {
    ast->accept(this);
}
void CodeGen::FunctionCodeGen::StmtCodeGen::visitExprStmt(ASTNS::ExprStmt *ast) {
    fcg.exprCG.expr(ast->expr.get());
}
void CodeGen::FunctionCodeGen::StmtCodeGen::visitVarStmt(ASTNS::VarStmt *ast) {
    ast->assignments->accept(this);
}
void CodeGen::FunctionCodeGen::StmtCodeGen::visitRetStmt(ASTNS::RetStmt *ast) {
    IR::ASTValue v;
    if (ast->expr) {
        v = fcg.exprCG.expr(ast->expr.get());
        if (!v)
            return;
    } else
        v = IR::ASTValue(cg.context->getVoid(), ast);

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

void CodeGen::FunctionCodeGen::StmtCodeGen::visitStmtList(ASTNS::StmtList *ast) {
    for (std::unique_ptr<ASTNS::Stmt> &s : ast->stmts)
        s->accept(this);
}

