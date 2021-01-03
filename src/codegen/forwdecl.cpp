#include "codegenlocal.h"
#include "message/errmsgs.h"
#include "ir/unit.h"

CodeGen::ForwDecl::ForwDecl(CodeGen &cg): cg(cg) {}

void CodeGen::ForwDecl::visitCU(ASTNS::CU *ast) {
    ast->decls->accept(this);
}

void CodeGen::ForwDecl::visitDeclList(ASTNS::DeclList *ast) {
    for (std::unique_ptr<ASTNS::Decl> &decl : ast->decls)
        decl->accept(this);
}

void CodeGen::ForwDecl::visitFunctionDecl(ASTNS::FunctionDecl *fun) {}
void CodeGen::ForwDecl::visitImplDecl(ASTNS::ImplDecl *fun) {}

void CodeGen::ForwDecl::visitImplicitDecl(ASTNS::ImplicitDecl *) {}
