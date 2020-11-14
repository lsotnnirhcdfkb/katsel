#include "codegen/codegen.h"
#include "message/errors.h"

CodeGenNS::StmtCodeGen::StmtCodeGen(CodeGen &cg): cg(cg) {}

void CodeGenNS::StmtCodeGen::stmt(ASTNS::StmtB *ast)
{
    ast->accept(this);
}

void CodeGenNS::StmtCodeGen::visitBlock(ASTNS::Block *ast)
{
    cg.context.incScope();
    ast->stmts->accept(this);
    cg.context.decScope();
}
void CodeGenNS::StmtCodeGen::visitExprStmt(ASTNS::ExprStmt *ast)
{
    // TODO
}
void CodeGenNS::StmtCodeGen::visitRetStmt(ASTNS::RetStmt *ast)
{
    // TODO
}
void CodeGenNS::StmtCodeGen::visitVarStmt(ASTNS::VarStmt *ast)
{
    // TODO
}

void CodeGenNS::StmtCodeGen::visitStmt(ASTNS::Stmt *) {}
void CodeGenNS::StmtCodeGen::visitEmptyStmt(ASTNS::EmptyStmt *) {}
void CodeGenNS::StmtCodeGen::visitStmts(ASTNS::Stmts *ast)
{
    ast->stmts->accept(this);
    ast->stmt->accept(this);
}

