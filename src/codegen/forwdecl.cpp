#include "codegenlocal.h"
#include "message/errmsgs.h"
#include "ir/unit.h"

CodeGen::ForwDecl::ForwDecl(CodeGen &cg): cg(cg) {}

void CodeGen::ForwDecl::visitCU(NNPtr<ASTNS::CU> ast) {
    for (std::unique_ptr<ASTNS::Decl> &decl : ast->decls)
        decl->accept(this);
}

void CodeGen::ForwDecl::visitFunctionDecl(NNPtr<ASTNS::FunctionDecl> fun) {}
void CodeGen::ForwDecl::visitImplDecl(NNPtr<ASTNS::ImplDecl> impl) {}

void CodeGen::ForwDecl::visitImplicitDecl(NNPtr<ASTNS::ImplicitDecl>) {}
