#include "codegen/codegen.h"

#include "llvm/Support/raw_ostream.h"

CodeGen::CodeGen::CodeGen(std::string const &name) : context(name), declarator(*this), typeResolver(*this) {}

void CodeGen::CodeGen::declarate(ASTNS::Decls *decls)
{
    decls->accept(&declarator);
}

void CodeGen::CodeGen::codegen(ASTNS::Decls *decls) {}

void CodeGen::CodeGen::printMod()
{
    context.mod->print(llvm::outs(), nullptr);
}
