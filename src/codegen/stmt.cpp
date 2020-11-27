#include "codegen/codegen.h"
#include "message/errors.h"
#include "message/errmsgs.h"

CodeGenNS::StmtCodeGen::StmtCodeGen(CodeGen &cg): cg(cg) {}

void CodeGenNS::StmtCodeGen::stmt(ASTNS::StmtB *ast)
{
    ast->accept(this);
}

void CodeGenNS::StmtCodeGen::visitBlock(ASTNS::Block *ast)
{
    cg.context.incScope();
    if (ast->stmts)
        ast->stmts->accept(this);
    cg.context.decScope();
}
void CodeGenNS::StmtCodeGen::visitExprStmt(ASTNS::ExprStmt *ast)
{
    cg.exprCodeGen.expr(ast->expr.get());
}
void CodeGenNS::StmtCodeGen::visitRetStmt(ASTNS::RetStmt *ast)
{
    if (ast->expr)
    {
        IR::ASTValue v = cg.exprCodeGen.expr(ast->expr.get());
        if (!v)
            return;

        if (!cg.context.retReg)
        {
            ERR_RET_VAL_VOID_FUN(v, cg.context.curFunc);
            cg.errored = true;
            return;
        }

        if (cg.context.retReg->type() != v.type())
        {
            ERR_CONFLICT_RET_TY(v, cg.context.curFunc);
            cg.errored = true;
            return;
        }

        cg.context.curBlock->add(std::make_unique<IR::Instrs::Store>(cg.context.retReg, v));
    }
    else if (cg.context.retReg)
    {
        ERR_RET_VOID_NONVOID_FUN(ast, cg.context.curFunc);
        cg.errored = true;
        return;
    }

    cg.context.curBlock->branch(std::make_unique<IR::Instrs::GotoBr>(cg.context.exitBlock));
    cg.context.curBlock = cg.context.blackHoleBlock.get();
}
void CodeGenNS::StmtCodeGen::visitVarStmt(ASTNS::VarStmt *ast)
{
    IR::Type *ty = cg.typeResolver.type(ast->type.get());

    varty = ty;
    ast->assignments->accept(this);
    varty = nullptr;
}

void CodeGenNS::StmtCodeGen::visitEmptyStmt(ASTNS::EmptyStmt *) {}
void CodeGenNS::StmtCodeGen::visitStmtList(ASTNS::StmtList *ast)
{
    ast->stmtlist->accept(this);
    ast->stmt->accept(this);
}

