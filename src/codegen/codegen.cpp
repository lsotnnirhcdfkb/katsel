#include "codegenlocal.h"
#include "ir/unit.h"

CodeGen::CodeGen(File const &file)
    : unit(std::make_unique<IR::Unit>(file)),
      context(std::make_unique<Context>()),
      typeVisitor(std::make_unique<TypeVisitor>(*this)) {}
CodeGen::~CodeGen() = default;

void CodeGen::declarate(ASTNS::CUB *cub)
{
    ForwDecl f (*this);
    cub->accept(&f);
}

void CodeGen::codegen(ASTNS::CUB *cub)
{

}

void CodeGen::printUnit(llvm::raw_ostream &ostream)
{
    unit->print(ostream);
}
