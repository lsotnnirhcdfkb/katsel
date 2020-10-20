#include "codegen/codegen.h"
#include "message/errors.h"
#include "message/fmtmessage.h"

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
    if (a->val)
    {
        Value v = evalExpr(a->val.get());
        if (v.val)
            context.builder.CreateRet(v.val);
    }
    else
        context.builder.CreateRetVoid();
}

void CodeGen::visitVarStmt(ASTNS::VarStmt *a)
{
    std::string varname = tokenToStr(a->name);
    Type *ty = evalType(a->type.get());
    if (dynamic_cast<VoidType*>(ty))
    {
        report(MsgType::ERROR, msg::voidVarNotAllowed(), a->name, a->type.get(), a->name);
        return;
    }

    Local *var = context.findLocal(varname);
    if (var && var->scopenum == context.curScope)
    {
        report(MsgType::ERROR, msg::cannotRedefineVariable(), a->name, a->name);
        return;
    }

    llvm::Function *f = context.builder.GetInsertBlock()->getParent();
    llvm::AllocaInst *alloca = context.createEntryAlloca(f, ty->toLLVMType(context.context), varname);
    context.addLocal(varname, ty, alloca);

    a->assign->accept(this);
}
