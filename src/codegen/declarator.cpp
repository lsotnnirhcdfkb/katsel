#include "codegenlocal.h"
#include "message/errmsgs.h"
#include "ir/unit.h"

CodeGen::Declarator::Declarator(CodeGen &cg): cg(cg), currentSymbol(&cg.unit->mod), thisType(Maybe<NNPtr<IR::Type>>()) {}

void CodeGen::Declarator::visitCU(ASTNS::CU &ast) {
    for (std::unique_ptr<ASTNS::Decl> &decl : ast.decls)
        decl->accept(*this);
}

void CodeGen::Declarator::visitFunctionDecl(ASTNS::FunctionDecl &fun) {
    std::string fname (fun.name.stringify());
    Maybe<NNPtr<IR::Value>> declbefore = currentSymbol->getValue(fname);

    if (declbefore.has()) {
        ERR_REDECL_SYM(fun.name, declbefore.get());
        cg.errored = true;
        return;
    }

    Maybe<NNPtr<IR::Type>> m_retty = cg.typeVisitor->type(fun.retty.get(), thisType);
    if (!m_retty.has())
        return;

    NNPtr<IR::Type> retty = m_retty.get();

    CodeGen::ParamVisitor p (cg, fun.params, thisType);
    std::vector<CodeGen::ParamVisitor::Param> params (std::move(p.ret));

    std::vector<NNPtr<IR::Type>> ptys;
    for (auto const &p : params)
        ptys.push_back(p.ty);

    NNPtr<IR::FunctionType> ft = cg.context->getFunctionType(retty, ptys);

    std::unique_ptr<IR::Function> f = std::make_unique<IR::Function>(ft, fname, fun);
    NNPtr<IR::Function> fraw = f.get();
    cg.unit->functions.push_back(std::move(f));
    currentSymbol->addValue(fname, fraw);

    if (p.isMethod) {
        thisType.get()->addMethod(fname, IR::Type::Method { fraw, p.thisPtr, p.thisMut });
    }
}

void CodeGen::Declarator::visitImplDecl(ASTNS::ImplDecl &impl) {
    Maybe<NNPtr<IR::Type>> m_implFor = cg.typeVisitor->type(impl.implFor.get(), Maybe<NNPtr<IR::Type>>());
    if (!m_implFor.has()) {
        cg.errored = true;
        ERR_UNDECL_SYMB(NNPtr<ASTNS::Type>(impl.implFor.get()));
        return;
    }

    NNPtr<IR::Type> implFor = m_implFor.get();

    NNPtr<IR::DeclSymbol> oldSymbol = currentSymbol;
    currentSymbol = implFor;
    thisType = implFor;
    for (std::unique_ptr<ASTNS::ImplMember> &member : impl.members) {
        member->accept(*this);
    }
    currentSymbol = oldSymbol;
    thisType = Maybe<NNPtr<IR::Type>>();
}

void CodeGen::Declarator::visitFunctionImplMember(ASTNS::FunctionImplMember &member) {
    member.fun->accept(*this);
}

void CodeGen::Declarator::visitImplicitDecl(ASTNS::ImplicitDecl &) {}
