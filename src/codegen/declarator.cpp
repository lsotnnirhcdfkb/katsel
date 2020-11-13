#include "codegen/codegen.h"

CodeGen::Declarator::Declarator(CodeGen &cg): cg(cg) {}

void CodeGen::Declarator::visitDecls(ASTNS::Decls *ast)
{
    ast->decls->accept(this);
    ast->decl->accept(this);
}

void CodeGen::Declarator::visitDecl(ASTNS::Decl *) {}

void CodeGen::Declarator::visitFunction(ASTNS::Function *fun)
{
    Type *retty;
}
