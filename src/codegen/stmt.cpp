#include "codegen/codegen.h"
#include "message/errors.h"

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
        if (!v.val)
            CG_RETURNNULL();

        FunctionType *fty = dynamic_cast<FunctionType*>(context.curFunc.type);
        Value vconv = fty->ret->castTo(context, v);
        if (!vconv.val)
            context.builder.CreateRet(vconv.val);
    }
    else
        context.builder.CreateRetVoid();
}

void CodeGen::visitVarStmt(ASTNS::VarStmt *a)
{
    Type *ty = evalType(a->type.get());
    if (dynamic_cast<VoidType*>(ty))
    {
        Error(Error::MsgType::ERROR, a->type.get(), "Invalid variable type \"void\"")
            .primary(Error::Primary(a->type.get())
                .error("Variable cannot be of type void"))
            .report();
        
        CG_RETURNNULL();
    }

    for (std::unique_ptr<ASTNS::Expr> const &a : a->assignments)
    {
        ASTNS::BinaryExpr *asb;
        ASTNS::PrimaryExpr *asp;
        Token nametok;
        if ((asb = dynamic_cast<ASTNS::BinaryExpr*>(a.get())))
        {
            if (asb->op.type != TokenType::EQUAL)
                Error(Error::MsgType::INTERR, asb, "Assignment expression does not have operator =")
                    .primary(Error::Primary(asb->op)
                        .error("not ="))
                    .secondary(asb)
                    .reportAbort();

            ASTNS::PrimaryExpr *primary (dynamic_cast<ASTNS::PrimaryExpr*>(asb->lhs.get()));
            if (!primary)
                Error(Error::MsgType::INTERR, asb->lhs.get(), "lhs of assignment is not of type PrimaryExpr")
                    .primary(Error::Primary(asb->lhs.get())
                        .error("not primary"))
                    .secondary(asb)
                    .reportAbort();

            nametok = primary->value;
        }
        else if ((asp = dynamic_cast<ASTNS::PrimaryExpr*>(a.get())))
            nametok = asp->value;
        else
            Error(Error::MsgType::INTERR, a.get(), "expr is not of type BinaryExpr or PrimaryExpr")
                .primary(Error::Primary(a.get())
                    .error("not binary or primary"))
                .reportAbort();

        std::string varname = tokenToStr(nametok);
        Local *var = context.findLocal(varname);
        if (var && var->scopenum == context.curScope)
        {
            Error(Error::MsgType::ERROR, nametok, "Duplicate variable")
                .primary(Error::Primary(nametok)
                    .error("Duplicate variable"))
                .primary(Error::Primary(var->v.ast)
                    .note("previous declaration is here"))
                .report();
            CG_RETURNNULL();
        }

        llvm::Function *f = context.builder.GetInsertBlock()->getParent();
        llvm::AllocaInst *alloca = context.createEntryAlloca(f, ty->toLLVMType(context.context), varname);
        context.addLocal(varname, ty, alloca, a.get());

        if (asb)
            asb->accept(this);
    }
}
