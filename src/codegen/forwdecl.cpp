#include "codegenlocal.h"
#include "message/errmsgs.h"
#include "ir/unit.h"

CodeGen::ForwDecl::ForwDecl(CodeGen &cg): cg(cg) {}

void CodeGen::ForwDecl::visitCU(ASTNS::CU *ast) {
    // ('void', 'VOID'),
    // ('float', 'FLOAT'),
    // ('bool', 'BOOL'),
    // ('double', 'DOUBLE'),
    // ('char', 'CHAR'),
    // ('uint8', 'UINT8'),
    // ('uint16', 'UINT16'),
    // ('uint32', 'UINT32'),
    // ('uint64', 'UINT64'),
    // ('sint8', 'SINT8'),
    // ('sint16', 'SINT16'),
    // ('sint32', 'SINT32'),
    // ('sint64', 'SINT64'),
    ast->decls->accept(this);
}

void CodeGen::ForwDecl::visitDeclList(ASTNS::DeclList *ast) {
    for (std::unique_ptr<ASTNS::Decl> &decl : ast->decls)
        decl->accept(this);
}

void CodeGen::ForwDecl::visitFunctionDecl(ASTNS::FunctionDecl *fun) {
    std::string fname (fun->name.stringify());
    IR::Value *declbefore = cg.context->getGlobal(fname);

    if (declbefore) {
        ERR_REDECL_SYM(fun->name, declbefore);
        cg.errored = true;
        return;
    }

    IR::Type *retty = cg.typeVisitor->type(fun->retty.get());
    if (!retty)
        return;

    std::vector<CodeGen::ParamVisitor::Param> params;
    if (fun->params) {
        CodeGen::ParamVisitor p (cg);
        fun->params->accept(&p);
        params = p.ret;
    }

    std::vector<IR::Type*> ptys;
    for (auto const &p : params)
        ptys.push_back(p.ty);

    IR::FunctionType *ft = cg.context->getFunctionType(retty, ptys);
    IR::Function *f = cg.unit->addFunction(ft, fname, fun);

    cg.context->addGlobal(fname, f);
}

void CodeGen::ForwDecl::visitImplicitDecl(ASTNS::ImplicitDecl *) {}
