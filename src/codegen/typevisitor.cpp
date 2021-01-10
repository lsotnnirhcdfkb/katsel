#include "codegenlocal.h"
#include "message/errmsgs.h"
#include "ir/unit.h"

CodeGen::TypeVisitor::TypeVisitor(CodeGen &cg): cg(cg) {}

IR::Type* CodeGen::TypeVisitor::type(ASTNS::Type *ast, IR::Type *thisType) {
    IR::Type *oldret = ret;
    ret = nullptr;
    this->thisType = thisType;

    ast->accept(this);

    IR::Type *newret = ret;
    ret = oldret;
    this->thisType = nullptr;

    return newret;
}

void CodeGen::TypeVisitor::visitPathType(ASTNS::PathType *ast) {
    IR::DeclSymbol *decl = cg.pathVisitor->resolveDeclSymbol(ast->path.get());

    if (!decl) {
        ret = nullptr;
        cg.errored = true;
        return;
    }

    if (!(ret = dynamic_cast<IR::Type*>(decl))) {
        ERR_NOT_A_TYPE(ast->path.get(), decl->declAST());
        cg.errored = true;
        return;
    }
}

void CodeGen::TypeVisitor::visitPointerType(ASTNS::PointerType *ast) {
    IR::Type *ty = type(ast->type.get(), thisType);
    if (!ty) {
        cg.errored = true;
        ret = nullptr;
        return;
    } else {
        ret = cg.context->getPointerType(ast->mut, ty);
    }
}

void CodeGen::TypeVisitor::visitThisType(ASTNS::ThisType *ast) {
    if (!thisType) {
        ERR_NO_THIS(ast->th);
        cg.errored = true;
        ret = nullptr;
        return;
    }

    ret = thisType;
}
