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
        Value *v = cg.exprCodeGen.expr(ast->expr.get());
        if (!v)
            return;

        if (!cg.context.retReg)
        {
            Error(Error::MsgType::ERROR, v, "cannot return value from function with return type \"void\"")
                .underline(Error::Underline(v, '^')
                    .error(concatMsg("returning ", v->type()->stringify(), " here")))
                .underline(Error::Underline(static_cast<ASTNS::Function*>(cg.context.curFunc->ast())->retty.get(), '-')
                    .note("returns void"))
                .report();
            return;
        }

        if (cg.context.retReg->type() != v->type())
        {
            Error(Error::MsgType::ERROR, v, "cannot return value of different type than expected return value")
                .underline(Error::Underline(v, '^')
                    .error(concatMsg("returning ", v->type()->stringify(), " here")))
                .underline(Error::Underline(static_cast<ASTNS::Function*>(cg.context.curFunc->ast())->retty.get(), '-')
                    .note(concatMsg("function returns ", cg.context.retReg->type()->stringify())))
                .report();
            return;
        }

        cg.context.curBlock->add(std::make_unique<Instrs::Store>(cg.context.retReg, v));
    }
    else if (cg.context.retReg)
    {
        Error(Error::MsgType::ERROR, ast, "return from non-void function must return a value")
            .underline(Error::Underline(ast, '^')
                .error("returning nothing here"))
            .underline(Error::Underline(static_cast<ASTNS::Function*>(cg.context.curFunc->ast())->retty.get(), '-')
                .note(concatMsg("function returns ", cg.context.retReg->type()->stringify())))
            .report();
        return;
    }

    cg.context.curBlock->branch(std::make_unique<Instrs::GotoBr>(cg.context.exitBlock));
    cg.context.curBlock = cg.context.blackHoleBlock.get();
}
void CodeGenNS::StmtCodeGen::visitVarStmt(ASTNS::VarStmt *ast)
{
    Type *ty = cg.typeResolver.type(ast->type.get());

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

