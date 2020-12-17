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
#define CASE(ty) case TokenType::ty: ret = 
#define GET(ty) cg.context->get##ty
        CASE(UINT8) GET(IntType)(8, false); return;
        CASE(UINT16) GET(IntType)(16, false); return;
        CASE(UINT32) GET(IntType)(32, false); return;
        CASE(UINT64) GET(IntType)(64, false); return;
        CASE(SINT8) GET(IntType)(8, true); return;
        CASE(SINT16) GET(IntType)(16, true); return;
        CASE(SINT32) GET(IntType)(32, true); return;
        CASE(SINT64) GET(IntType)(64, true); return;

        CASE(FLOAT) GET(FloatType)(32); return;
        CASE(DOUBLE) GET(FloatType)(64); return;
        CASE(BOOL) GET(BoolType)(); return;
        CASE(CHAR) GET(CharType)(); return;
        CASE(VOID) GET(VoidType)(); return;
#undef CASE
#undef GET

        default:
            invalidTok("builtin type", ast->type);
    }
}
