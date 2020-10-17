#include "codegen/codegen.h"

#include "llvm/IR/Verifier.h"

void CodeGen::visitFunctionDecl(ASTNS::FunctionDecl *a)
{
    std::string name = tokenToStr(a->name);
    llvm::Function *f = context.mod->getFunction(name);

    if (!f->empty())
        return;

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

    a->block->accept(this);
    llvm::verifyFunction(*f);
    context.decScope();
}

void CodeGen::visitGlobalVarDecl(ASTNS::GlobalVarDecl *a)
{
    // TODO: this
    return;
}
