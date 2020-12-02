
#include "codegen/codegen.h"
#include "message/errors.h"

CodeGenNS::TypeResolve::TypeResolve(CodeGen &cg): cg(cg) {}

IR::Type* CodeGenNS::TypeResolve::type(ASTNS::TypeB *ast) {
    ret = nullptr;
    ast->accept(this);
    return ret;
}

void CodeGenNS::TypeResolve::visitBuiltinTypeNoVoid(ASTNS::BuiltinTypeNoVoid *ast)
{
    switch (ast->type.type)
    {
#define TY(ty) case TokenType::ty: ret = cg.context.getBuiltinType(IR::BuiltinType::Builtins::ty); return;
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
        default:
            invalidTok("builtin type", ast->type);
    }
}

void CodeGenNS::TypeResolve::visitTypeV(ASTNS::TypeV *ast)
{
    ret = cg.context.getVoidType();
}
