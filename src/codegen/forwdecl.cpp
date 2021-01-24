#include "codegenlocal.h"
#include "message/errmsgs.h"
#include "ir/unit.h"
#include "ast/ast.h"

CodeGen::ForwDecl::ForwDecl(CodeGen &cg): cg(cg) {}

void CodeGen::ForwDecl::visit_cu(ASTNS::CU &ast) {
    for (std::unique_ptr<ASTNS::Decl> &decl : ast.decls)
        decl->accept(*this);
}

void CodeGen::ForwDecl::visit_function_decl(ASTNS::FunctionDecl &fun) {}
void CodeGen::ForwDecl::visit_impl_decl(ASTNS::ImplDecl &impl) {}

void CodeGen::ForwDecl::visit_implicit_decl(ASTNS::ImplicitDecl &) {}
