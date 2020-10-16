#include "codegen/codegen.h"
#include "lex/tokentype.h"

#include "llvm/IR/Type.h"

void CodeGen::visitBaseType(ASTNS::BaseType *a)
{
	switch (a->type.type)
    {
#define BUILTINTYPE(ty) case TokenType::ty: typeRetVal = context.getBuiltinType(BuiltinType::Builtins::ty); break;
        BUILTINTYPE(UINT8)
        BUILTINTYPE(SINT8)
        BUILTINTYPE(CHAR)
        BUILTINTYPE(UINT16)
        BUILTINTYPE(SINT16)
        BUILTINTYPE(SINT32)
        BUILTINTYPE(UINT32)
        BUILTINTYPE(SINT64)
        BUILTINTYPE(UINT64)
        BUILTINTYPE(FLOAT)
        BUILTINTYPE(DOUBLE)
        BUILTINTYPE(BOOL)
        BUILTINTYPE(VOID)
#undef BUILTINTYPE
    }
}
