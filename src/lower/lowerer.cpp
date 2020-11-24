#include "lower/lowerer.h"
#include <memory>

#include "llvm/IR/DerivedTypes.h"

Lower::Lowerer::Lowerer(IR::Unit const &unit): unit(unit), builder(context), mod(unit.file.filename, context) {}

void Lower::Lowerer::printMod(std::ostream &ostream)
{
    mod.print(llvm::outs(), nullptr);
}

void Lower::Lowerer::lower()
{
    for (std::unique_ptr<IR::Function> const &f : unit.functions)
        lower(*f);
}


void Lower::Lowerer::lower(IR::Function const &f)
{
    auto *fty = static_cast<llvm::FunctionType*>(f.ty->toLLVMType(context));
    auto *fllvm = llvm::Function::Create(fty, llvm::Function::ExternalLinkage, f.name, &mod);

    for (std::unique_ptr<IR::Register> const &r : f.registers)
        ;
    for (std::unique_ptr<IR::Block> const &b : f.blocks)
        ;
    for (std::unique_ptr<IR::Block> const &b : f.blocks)
        lower(*b);

    allocas.clear();
}

void Lower::Lowerer::lower(IR::Block const &b)
{

}
