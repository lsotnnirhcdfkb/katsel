#include "codegen/codegen.h"
#include "message/errors.h"
#include "message/errmsgs.h"

CodeGenNS::StmtCodeGen::StmtCodeGen(CodeGen &cg): cg(cg) {}

void CodeGenNS::StmtCodeGen::stmt(ASTNS::StmtB *ast)
{
    ast->accept(this);
}
void CodeGenNS::StmtCodeGen::visitExprStmt(ASTNS::ExprStmt *ast)
{
    cg.exprCodeGen.expr(ast->expr.get());
}
void CodeGenNS::StmtCodeGen::visitVarStmt(ASTNS::VarStmt *ast)
{
    IR::Type *ty = cg.typeResolver.type(ast->type.get());

    varty = ty;
    ast->assignments->accept(this);
    varty = nullptr;
}
void CodeGenNS::StmtCodeGen::visitRetStmt(ASTNS::RetStmt *ast)
{
    IR::ASTValue v;
    if (ast->expr)
    {
        v = cg.exprCodeGen.expr(ast->expr.get());
        if (!v)
            return;
    }
    else
        v = IR::ASTValue(cg.context.getVoidValue(), ast);

    if (cg.context.retReg->type() != v.type())
    {
        ERR_CONFLICT_RET_TY(v, cg.context.curFunc);
        cg.errored = true;
        return;
    }

    cg.context.curBlock->add(std::make_unique<IR::Instrs::Store>(cg.context.retReg, v));
    cg.context.curBlock->branch(std::make_unique<IR::Instrs::GotoBr>(cg.context.exitBlock));
    cg.context.curBlock = cg.context.blackHoleBlock.get();
}

void CodeGenNS::StmtCodeGen::visitStmtList_OPT(ASTNS::StmtList_OPT *ast) {}
void CodeGenNS::StmtCodeGen::visitStmtList(ASTNS::StmtList *ast)
{
    ast->stmtlist->accept(this);
    ast->anotherstmt->accept(this);
}

