#include "codegen/codegen.h"
#include "message/errors.h"

#include "llvm/IR/Verifier.h"

void CodeGen::visitFunctionDecl(ASTNS::FunctionDecl *a)
{
    std::string name = tokenToStr(a->name);
    Value function = context.getGlobal(name);
    if (!dynamic_cast<FunctionType*>(function.type))
        reportAbortNoh("In CodeGen::visitFunctionDecl, context.getGlobal(" + name + ") returned non-function");

    llvm::Value *fv = function.val;
    llvm::Function *f = static_cast<llvm::Function*>(fv);

    if (!f->empty())
        CG_RETURNNULL();

    llvm::BasicBlock *block = llvm::BasicBlock::Create(context.context, name + "Entry", f);
    context.builder.SetInsertPoint(block);

    context.incScope();

    ASTNS::Param *paramast = a->params.get();
    for (auto &param : f->args())
    {
        std::string pname = tokenToStr(paramast->name);
        llvm::AllocaInst *alloca = context.createEntryAlloca(f, param.getType(), pname);

        context.builder.CreateStore(&param, alloca);

        context.addLocal(pname, evalType(paramast->type.get()), alloca);

        paramast = paramast->next.get();
    }

    context.curFunc = function;
    a->block->accept(this);
    llvm::verifyFunction(*f);
    context.decScope();
}

void CodeGen::visitGlobalVarDecl(ASTNS::GlobalVarDecl *a)
{
    // TODO: this
    return;
}
