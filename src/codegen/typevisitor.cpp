#include "codegenlocal.h"
#include "message/internal.h"
#include "message/errmsgs.h"

CodeGen::TypeVisitor::TypeVisitor(CodeGen &cg): cg(cg) {}

IR::Type* CodeGen::TypeVisitor::type(ASTNS::Type *ast) {
    IR::Type *oldret = ret;
    ret = nullptr;

    ast->accept(this);

    IR::Type *newret = ret;
    ret = oldret;

    return newret;
}

void CodeGen::TypeVisitor::visitPrimitiveType(ASTNS::PrimitiveType *ast) {
    ret = cg.context->getType(ast->ty.stringify());

    if (!ret)
        ERR_UNDECL_TYPE(ast->ty);
}

void CodeGen::TypeVisitor::visitPointerType(ASTNS::PointerType *ast) {
    IR::Type *ty = type(ast->type.get());
    ret = cg.context->getPointerType(ty);
}
