#include "codegenlocal.h"
#include "message/errors.h"
#include "message/errmsgs.h"

CodeGen::FunctionCodeGen::StmtCodeGen::StmtCodeGen(CodeGen &cg, FunctionCodeGen &fcg): cg(cg), fcg(fcg) {}

void CodeGen::FunctionCodeGen::StmtCodeGen::stmt(ASTNS::StmtB *ast)
{
    ast->accept(this);
}
void CodeGen::FunctionCodeGen::StmtCodeGen::visitExprStmt(ASTNS::ExprStmt *ast)
{
    fcg.exprCG.expr(ast->expr.get());
}
void CodeGen::FunctionCodeGen::StmtCodeGen::visitVarStmt(ASTNS::VarStmt *ast)
{
    IR::Type *ty = cg.typeVisitor->type(ast->type.get());

    IR::Type *oldvarty = varty;
    varty = ty;
    ast->assignments->accept(this);
    varty = oldvarty;
}
void CodeGen::FunctionCodeGen::StmtCodeGen::visitVarStmt_OPT(ASTNS::VarStmt_OPT *ast) {}
void CodeGen::FunctionCodeGen::StmtCodeGen::visitRetStmt(ASTNS::RetStmt *ast)
{
    IR::ASTValue v;
    if (ast->expr)
    {
        v = fcg.exprCG.expr(ast->expr.get());
        if (!v)
            return;
    }
    else
        v = IR::ASTValue(cg.context->getVoid(), ast);

    if (fcg.ret->type() != v.type())
    {
        ERR_CONFLICT_RET_TY(v, fcg.fun);
        cg.errored = true;
        return;
    }

    fcg.curBlock->add(std::make_unique<IR::Instrs::Store>(fcg.ret, v));
    fcg.curBlock->branch(std::make_unique<IR::Instrs::GotoBr>(fcg.exitBlock));
    // fcg.curBlock = cg.context.blackHoleBlock.get();
}

void CodeGen::FunctionCodeGen::StmtCodeGen::visitStmtList_OPT(ASTNS::StmtList_OPT *ast) {}
void CodeGen::FunctionCodeGen::StmtCodeGen::visitStmtList(ASTNS::StmtList *ast)
{
    ast->stmtlist->accept(this);
    ast->anotherstmt->accept(this);
}

