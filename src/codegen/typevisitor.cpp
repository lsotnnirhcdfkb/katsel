#include "codegenlocal.h"
#include "message/internal.h"

CodeGen::TypeVisitor::TypeVisitor(CodeGen &cg): cg(cg) {}

IR::Type* CodeGen::TypeVisitor::type(ASTNS::TypeB *ast) {
    IR::Type *oldret = ret;
    ret = nullptr;

    ast->accept(this);

    IR::Type *newret = ret;
    ret = oldret;

    return newret;
}

void CodeGen::TypeVisitor::visitBuiltinType(ASTNS::BuiltinType *ast)
{
    switch (ast->type.type)
    {
#define TY(ty) case TokenType::ty: ret = cg.context->getBuiltinType(IR::BuiltinType::Builtins::ty); return;
        TY(UINT8)
        TY(UINT16)
        TY(UINT32)
        TY(UINT64)
        TY(SINT8)
        TY(SINT16)
        TY(SINT32)
        TY(SINT64)
        TY(FLOAT)
        TY(DOUBLE)
        TY(BOOL)
        TY(CHAR)
#undef TY
        case TokenType::VOID: ret = cg.context->getVoidType(); return;

        default:
            invalidTok("builtin type", ast->type);
    }
}
