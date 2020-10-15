#include "codegen/globalsassembler.h"

GlobalsAssembler::GlobalsAssembler(CodeGenContext &con): context(con) {}

void GlobalsAssembler::visitProgram(ASTNS::Program *a)
{
    for (std::unique_ptr<ASTNS::Decl> &d : a->decls)
        d->accept(this);
}

void GlobalsAssembler::visitFunctionDecl(ASTNS::FunctionDecl *a)
{
}

void GlobalsAssembler::visitGlobalVarDecl(ASTNS::GlobalVarDecl *a)
{
}
