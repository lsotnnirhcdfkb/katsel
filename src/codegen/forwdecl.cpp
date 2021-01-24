#include "codegenlocal.h"
#include "message/errmsgs.h"
#include "ir/unit.h"
#include "ast/ast.h"

CodeGen::ForwDecl::ForwDecl(CodeGen &cg): cg(cg) {}

void CodeGen::ForwDecl::visit(ASTNS::CU &ast) {
    for (std::unique_ptr<ASTNS::Decl> &decl : ast.decls)
        decl->accept(*this);
}

void CodeGen::ForwDecl::visit(ASTNS::FunctionDecl &fun) {}
void CodeGen::ForwDecl::visit(ASTNS::ImplDecl &impl) {}

void CodeGen::ForwDecl::visit(ASTNS::ImplicitDecl &) {}
