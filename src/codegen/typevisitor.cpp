#include "codegenlocal.h"
#include "message/errmsgs.h"
#include "ir/unit.h"

CodeGen::TypeVisitor::TypeVisitor(CodeGen &cg): cg(cg) {}

Maybe<NNPtr<IR::Type>> CodeGen::TypeVisitor::type(NNPtr<ASTNS::Type> ast, Maybe<NNPtr<IR::Type>> thisType) {
    Maybe<NNPtr<IR::Type>> oldret = ret;
    ret = Maybe<NNPtr<IR::Type>>();
    this->thisType = thisType;

    ast->accept(this);

    Maybe<NNPtr<IR::Type>> newret = ret;
    ret = oldret;
    this->thisType = Maybe<NNPtr<IR::Type>>();

    return newret;
}

void CodeGen::TypeVisitor::visitPathType(NNPtr<ASTNS::PathType> ast) {
    Maybe<NNPtr<IR::DeclSymbol>> m_decl = cg.pathVisitor->resolveDeclSymbol(ast->path.get());

    if (!m_decl.has()) {
        ret = Maybe<NNPtr<IR::Type>>();
        cg.errored = true;
        return;
    }
    NNPtr<IR::DeclSymbol> decl = m_decl.get();

    IR::Type *asType = dynamic_cast<IR::Type*>(decl.asRaw());
    if (!asType) {
        ret = Maybe<NNPtr<IR::Type>>();
        ERR_NOT_A_TYPE(NNPtr<ASTNS::Path>(ast->path.get()), NNPtr(decl->declAST()));
        cg.errored = true;
        return;
    } else {
        ret = Maybe(NNPtr<IR::Type>(asType));
    }
}

void CodeGen::TypeVisitor::visitPointerType(NNPtr<ASTNS::PointerType> ast) {
    Maybe<NNPtr<IR::Type>> ty = type(ast->type.get(), thisType);
    ty.match([this, &ast] (NNPtr<IR::Type> const &ty) {
            ret = Maybe(static_cast<NNPtr<IR::Type>>(cg.context->getPointerType(ast->mut, ty)));
        },
        [this]  {
            cg.errored = true;
            ret = Maybe<NNPtr<IR::Type>>();
        });
}

void CodeGen::TypeVisitor::visitThisType(NNPtr<ASTNS::ThisType> ast) {
    if (thisType.has()) {
        ret = Maybe(thisType);
    } else {
        ERR_NO_THIS(ast->th);
        cg.errored = true;
        ret = Maybe<NNPtr<IR::Type>>();
        return;
    }

    ret = thisType;
}
