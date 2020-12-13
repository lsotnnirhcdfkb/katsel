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

void CodeGenNS::StmtCodeGen::visitStmtList_OPT(ASTNS::StmtList_OPT *ast) {}
void CodeGenNS::StmtCodeGen::visitStmtList(ASTNS::StmtList *ast)
{
    ast->stmtsegment->accept(this);
}
void CodeGenNS::StmtCodeGen::visitStmtSegment(ASTNS::StmtSegment *ast)
{
    ast->stmtsegment->accept(this);
    ast->anotherstmt->accept(this);
}

