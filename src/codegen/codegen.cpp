#include "codegenlocal.h"
#include "ir/unit.h"

CodeGen::CodeGen(File const &file, ASTNS::CUB *cub)
: unit(std::make_unique<IR::Unit>(file)),
  context(std::make_unique<Context>(file, *this)),
  typeVisitor(std::make_unique<TypeVisitor>(*this)),
  pathVisitor(std::make_unique<PathVisitor>(*this)),
  errored(false),
  cub(cub) {}
CodeGen::~CodeGen() = default;

void CodeGen::forwdecl() {
    unit->mod.addDeclSymbol("void", context->getVoidType());
    unit->mod.addDeclSymbol("float", context->getFloatType(32));
    unit->mod.addDeclSymbol("double", context->getFloatType(64));
    unit->mod.addDeclSymbol("bool", context->getBoolType());
    unit->mod.addDeclSymbol("char", context->getCharType());
    unit->mod.addDeclSymbol("uint8", context->getIntType(8, false));
    unit->mod.addDeclSymbol("uint16", context->getIntType(16, false));
    unit->mod.addDeclSymbol("uint32", context->getIntType(32, false));
    unit->mod.addDeclSymbol("uint64", context->getIntType(64, false));
    unit->mod.addDeclSymbol("sint8", context->getIntType(8, true));
    unit->mod.addDeclSymbol("sint16", context->getIntType(16, true));
    unit->mod.addDeclSymbol("sint32", context->getIntType(32, true));
    unit->mod.addDeclSymbol("sint64", context->getIntType(64, true));

    ForwDecl f (*this);
    cub->accept(&f);
}

void CodeGen::declarate() {
    Declarator d (*this);
    cub->accept(&d);
}

void CodeGen::codegen() {
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

#define DECL_CG(cl) \
    void CodeGen::visit##cl##Decl(ASTNS::cl##Decl *ast) { \
        cl##CodeGen cg (*this, ast);                      \
        if (!cg.codegen())                                \
            errored = true;                               \
    }
DECL_CG(Function)
DECL_CG(Impl)

void CodeGen::visitImplicitDecl(ASTNS::ImplicitDecl *) {}
