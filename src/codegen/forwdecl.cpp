#include "codegenlocal.h"
#include "message/errmsgs.h"
#include "ir/unit.h"

CodeGen::ForwDecl::ForwDecl(CodeGen &cg): cg(cg) {}

void CodeGen::ForwDecl::visitCU(ASTNS::CU *ast) {
    cg.context->addType("void", cg.context->getVoidType());
    cg.context->addType("float", cg.context->getFloatType(32));
    cg.context->addType("double", cg.context->getFloatType(64));
    cg.context->addType("bool", cg.context->getBoolType());
    cg.context->addType("char", cg.context->getCharType());
    cg.context->addType("uint8", cg.context->getIntType(8, false));
    cg.context->addType("uint16", cg.context->getIntType(16, false));
    cg.context->addType("uint32", cg.context->getIntType(32, false));
    cg.context->addType("uint64", cg.context->getIntType(64, false));
    cg.context->addType("sint8", cg.context->getIntType(8, true));
    cg.context->addType("sint16", cg.context->getIntType(16, true));
    cg.context->addType("sint32", cg.context->getIntType(32, true));
    cg.context->addType("sint64", cg.context->getIntType(64, true));

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
