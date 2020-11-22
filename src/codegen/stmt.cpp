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
    Value *ret = nullptr;
    if (ast->expr)
    {
        Value *v = cg.exprCodeGen.expr(ast->expr.get());
        if (!v)
            return;

        FunctionType *fty = static_cast<FunctionType*>(cg.context.curFunc->type());
        if (fty->ret != v->type())
        {
            Error(Error::MsgType::ERROR, v, "cannot return value of different type than expected return value")
                .underline(Error::Underline(v, '^')
                    .error(concatMsg("returning ", v->type()->stringify(), " here")))
                .underline(Error::Underline(static_cast<ASTNS::Function*>(cg.context.curFunc->ast())->retty.get(), '-')
                    .note(concatMsg("function returns ", fty->ret->stringify())))
                .report();
            return;
        }

        ret = v;
    }

    cg.context.curBlock->branch(std::make_unique<Instrs::GotoBr>(cg.context.exitBlock)); // TODO: this does not work properly
    cg.context.curBlock = cg.context.exitBlock;
    cg.context.curBlock->add(std::make_unique<Instrs::Return>(ret));
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

