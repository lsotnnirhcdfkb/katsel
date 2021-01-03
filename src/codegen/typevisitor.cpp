#include "codegenlocal.h"
#include "message/errmsgs.h"
#include "ir/unit.h"

CodeGen::TypeVisitor::TypeVisitor(CodeGen &cg): cg(cg) {}

IR::Type* CodeGen::TypeVisitor::type(ASTNS::Type *ast) {
    IR::Type *oldret = ret;
    ret = nullptr;

    ast->accept(this);

    IR::Type *newret = ret;
    ret = oldret;

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
    IR::Type *ty = type(ast->type.get());
    if (!ty) {
        cg.errored = true;
        ret = nullptr;
        return;
    } else {
        ret = cg.context->getPointerType(ty);
    }
}
