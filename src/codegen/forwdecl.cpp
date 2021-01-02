#include "codegenlocal.h"
#include "message/errmsgs.h"
#include "ir/unit.h"

CodeGen::ForwDecl::ForwDecl(CodeGen &cg): cg(cg) {}

void CodeGen::ForwDecl::visitCU(ASTNS::CU *ast) {
    cg.unit->mod.addDeclSymbol("void", cg.context->getVoidType());
    cg.unit->mod.addDeclSymbol("float", cg.context->getFloatType(32));
    cg.unit->mod.addDeclSymbol("double", cg.context->getFloatType(64));
    cg.unit->mod.addDeclSymbol("bool", cg.context->getBoolType());
    cg.unit->mod.addDeclSymbol("char", cg.context->getCharType());
    cg.unit->mod.addDeclSymbol("uint8", cg.context->getIntType(8, false));
    cg.unit->mod.addDeclSymbol("uint16", cg.context->getIntType(16, false));
    cg.unit->mod.addDeclSymbol("uint32", cg.context->getIntType(32, false));
    cg.unit->mod.addDeclSymbol("uint64", cg.context->getIntType(64, false));
    cg.unit->mod.addDeclSymbol("sint8", cg.context->getIntType(8, true));
    cg.unit->mod.addDeclSymbol("sint16", cg.context->getIntType(16, true));
    cg.unit->mod.addDeclSymbol("sint32", cg.context->getIntType(32, true));
    cg.unit->mod.addDeclSymbol("sint64", cg.context->getIntType(64, true));

    ast->decls->accept(this);
}

void CodeGen::ForwDecl::visitDeclList(ASTNS::DeclList *ast) {
    for (std::unique_ptr<ASTNS::Decl> &decl : ast->decls)
        decl->accept(this);
}

void CodeGen::ForwDecl::visitFunctionDecl(ASTNS::FunctionDecl *fun) {
    std::string fname (fun->name.stringify());
    IR::Value *declbefore = cg.unit->mod.getValue(fname);

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

    std::unique_ptr<IR::Function> f = std::make_unique<IR::Function>(ft, fname, fun);
    IR::Function *fraw = f.get();
    cg.unit->functions.push_back(std::move(f));
    cg.unit->mod.addValue(fname, fraw);
}

void CodeGen::ForwDecl::visitImplicitDecl(ASTNS::ImplicitDecl *) {}
