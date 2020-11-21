#include "codegen/codegen.h"
#include "message/errors.h"

CodeGenNS::StmtCodeGen::StmtCodeGen(CodeGen &cg): cg(cg) {}

void CodeGenNS::StmtCodeGen::stmt(ASTNS::StmtB *ast) {}

void CodeGenNS::StmtCodeGen::visitBlock(ASTNS::Block *ast) {}
void CodeGenNS::StmtCodeGen::visitExprStmt(ASTNS::ExprStmt *ast) {}
void CodeGenNS::StmtCodeGen::visitRetStmt(ASTNS::RetStmt *ast) {}
void CodeGenNS::StmtCodeGen::visitVarStmt(ASTNS::VarStmt *ast) {}

void CodeGenNS::StmtCodeGen::visitStmt(ASTNS::Stmt *) {}
void CodeGenNS::StmtCodeGen::visitEmptyStmt(ASTNS::EmptyStmt *) {}
void CodeGenNS::StmtCodeGen::visitStmtList(ASTNS::StmtList *ast) {}

