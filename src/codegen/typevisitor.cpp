#include "codegenlocal.h"
#include "message/internal.h"
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

void CodeGen::TypeVisitor::visitPrimitiveType(ASTNS::PrimitiveType *ast) {
    IR::DeclSymbol *decl = cg.unit->mod.getDeclSymbol(ast->ty.stringify());

    if (!decl)
    {
        ERR_UNDECL_TYPE(ast->ty);
        cg.errored = true;
        return;
    }
    if (!(ret = dynamic_cast<IR::Type*>(decl)))
    {
        ERR_NOT_A_TYPE(ast->ty, decl->declAST());
        cg.errored = true;
        return;
    }
}

void CodeGen::TypeVisitor::visitPointerType(ASTNS::PointerType *ast) {
    IR::Type *ty = type(ast->type.get());
    ret = cg.context->getPointerType(ty);
}
