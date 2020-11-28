#include "lower/lowerer.h"
#include "message/errors.h"
#include <memory>

#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Verifier.h"

#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Utils.h"

Lower::Lowerer::Lowerer(IR::Unit const &unit): errored(false), unit(unit), builder(context), mod(unit.file.filename, context), fpm(&mod)
{
    fpm.add(llvm::createPromoteMemoryToRegisterPass());
    fpm.add(llvm::createInstructionCombiningPass());
    fpm.add(llvm::createReassociatePass());
    fpm.add(llvm::createGVNPass());
    fpm.add(llvm::createCFGSimplificationPass());

    fpm.doInitialization();
}

void Lower::Lowerer::printMod(std::ostream &ostream)
{
    mod.print(llvm::outs(), nullptr);
}

void Lower::Lowerer::lower()
{
    for (std::unique_ptr<IR::Function> const &f : unit.functions)
    {
        auto *fty = static_cast<llvm::FunctionType*>(f->ty->toLLVMType(context));
        auto *fllvm = llvm::Function::Create(fty, llvm::Function::ExternalLinkage, f->name, &mod);
        functions[f.get()] = fllvm;
    }
    for (std::unique_ptr<IR::Function> const &f : unit.functions)
        lower(*f);
}


void Lower::Lowerer::lower(IR::Function const &f)
{
    llvm::BasicBlock *entryBlock;

    llvm::Function *fasllvm = functions.at(&f);
    for (std::unique_ptr<IR::Block> const &b : f.blocks)
    {
        blocks[b.get()] = llvm::BasicBlock::Create(context, b->name, fasllvm);
        if (b->num == 0)
            entryBlock = blocks[b.get()];
    }

    if (!entryBlock)
        reportAbortNoh("entryBlock == nullptr");

    builder.SetInsertPoint(entryBlock);
    for (std::unique_ptr<IR::Register> const &r : f.registers)
        allocas[r.get()] = builder.CreateAlloca(r->type()->toLLVMType(context), 0, "");

    auto argiter = f.registers.begin();
    if (!dynamic_cast<IR::VoidType*>(f.ty->ret))
        ++argiter; // if return type is not void, then first register is reserved for return value

    for (auto &arg : fasllvm->args())
        builder.CreateStore(&arg, allocas[argiter++->get()]);

    for (std::unique_ptr<IR::Block> const &b : f.blocks)
        lower(*b);

    allocas.clear();
    blocks.clear();

    llvm::verifyFunction(*fasllvm);
    fpm.run(*fasllvm);
}

void Lower::Lowerer::lower(IR::Block const &b)
{
    builder.SetInsertPoint(blocks[&b]);
    for (std::unique_ptr<IR::Instrs::Instruction> const &i : b.instructions)
        i->accept(this);

    if (b.br)
        b.br->accept(this);
}

llvm::Value* Lower::Lowerer::lower(IR::Value const *v)
{
    if (!v)
        reportAbortNoh("lowerValue nullptr");

#define CHECKTY(ty) \
    IR::ty const *as##ty; \
    if ((as##ty = dynamic_cast<IR::ty const *>(v)))

    CHECKTY(Register)
        return builder.CreateLoad(allocas.at(asRegister));
    CHECKTY(Function)
        return functions.at(asFunction);
    CHECKTY(ConstInt)
        return llvm::ConstantInt::get(asConstInt->type()->toLLVMType(context), asConstInt->val);
#undef CHECKTY
    else
    {
        reportAbortNoh("lowerValue called with v of invlaid type");
    }
}

llvm::Value* Lower::Lowerer::lower(IR::ASTValue const &v)
{
    return lower(v.val);
}
