#include "codegenlocal.h"
#include "ir/unit.h"

CodeGen::CodeGen(File const &file)
: unit(std::make_unique<IR::Unit>(file)),
  context(std::make_unique<Context>(file)),
  typeVisitor(std::make_unique<TypeVisitor>(*this)),
  errored(false) {}
CodeGen::~CodeGen() = default;

void CodeGen::declarate(ASTNS::CUB *cub) {
    ForwDecl f (*this);
    cub->accept(&f);
}

void CodeGen::codegen(ASTNS::CUB *cub) {
    cub->accept(this);
}

void CodeGen::printUnit(llvm::raw_ostream &ostream) {
    unit->print(ostream);
}

// visiting {{{1
void CodeGen::visitCU(ASTNS::CU *ast) {
    ast->decls->accept(this);
}
void CodeGen::visitDeclList(ASTNS::DeclList *ast) {
    for (std::unique_ptr<ASTNS::Decl> &decl : ast->decls)
        decl->accept(this);
}
void CodeGen::visitFunctionDecl(ASTNS::FunctionDecl *ast) {
    FunctionCodeGen f (*this, ast);
    if (!f.codegen())
        errored = true;
}

void CodeGen::visitImplicitDecl(ASTNS::ImplicitDecl *) {}
