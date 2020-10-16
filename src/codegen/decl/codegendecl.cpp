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

    ASTNS::Param *paramast = a->params.get();
    for (auto &param : f->args())
    {
        llvm::AllocaInst *alloca = context.createEntryAlloca(f, param.getType(), param.getName());

        context.builder.CreateStore(&param, alloca);

        std::string pname = param.getName();
        context.addLocal(pname, evalType(paramast->type.get()), alloca);

        paramast = paramast->next.get();
    }

    a->block->accept(this);
    llvm::verifyFunction(*f);
}

void CodeGen::visitGlobalVarDecl(ASTNS::GlobalVarDecl *a)
{
    // TODO: this
    return;
}
