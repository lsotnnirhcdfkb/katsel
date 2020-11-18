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
        Value v = cg.exprCodeGen.expr(ast->expr.get());
        if (!v.val)
            return;

        FunctionType *fty = dynamic_cast<FunctionType*>(cg.context.curFunc.type);
        if (fty->ret != v.type)
        {
            Error(Error::MsgType::ERROR, v, "Cannot return value of different type than expected return value")
                .underline(Error::Underline(v, '^')
                    .error(concatMsg("returning ", v.type->stringify(), " here")))
                .underline(Error::Underline(static_cast<ASTNS::Function*>(cg.context.curFunc.ast)->retty.get(), '-')
                    .note(concatMsg("function returns ", fty->ret->stringify())))
                .report();
            return;
        }

        cg.context.builder.CreateRet(v.val);
    }
    else
        cg.context.builder.CreateRetVoid();
}
void CodeGenNS::StmtCodeGen::visitVarStmt(ASTNS::VarStmt *ast)
{
    Type *ty = cg.typeResolver.type(ast->type.get());

    varty = ty;
    ast->assignments->accept(this);
    varty = nullptr;
}

void CodeGenNS::StmtCodeGen::visitStmt(ASTNS::Stmt *) {}
void CodeGenNS::StmtCodeGen::visitEmptyStmt(ASTNS::EmptyStmt *) {}
void CodeGenNS::StmtCodeGen::visitStmtList(ASTNS::StmtList *ast)
{
    ast->stmtlist->accept(this);
    ast->stmt->accept(this);
}

