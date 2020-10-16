#include "codegen/codegen.h"

void CodeGen::visitBlockStmt(ASTNS::BlockStmt *a)
{
	context.incScope();
    for (std::unique_ptr<ASTNS::Stmt> &s : a->stmts)
        s->accept(this);
	context.decScope();
}
void CodeGen::visitExprStmt(ASTNS::ExprStmt *a)
{
	a->expr->accept(this);
}
void CodeGen::visitReturnStmt(ASTNS::ReturnStmt *a)
{
	// TODO: this
}

void CodeGen::visitVarStmt(ASTNS::VarStmt *a)
{
	// TODO: this
}
