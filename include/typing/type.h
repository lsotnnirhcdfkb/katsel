#pragma once

#include <vector>
#include "llvm/IR/Type.h"
#include "llvm/IR/LLVMContext.h"

#include <iostream>

enum TypeType
{
    BUILTIN,
    FUNCTION,
};

enum class BuiltinType
{
    UINT8,
    UINT16,
    UINT32,
    UINT64,
    SINT8,
    SINT16,
    SINT32,
    SINT64,

    FLOAT,
    CHAR,
    BOOL,
    DOUBLE,
    VOID
};

struct Type;

class FunctionType
{
public:
    Type *ret;
    std::vector<Type*> paramtys;

    ~FunctionType();
    FunctionType();
};

struct Type
{
    TypeType typetype;

    union aa // need destructor because FunctionType has non-trivial destructor
    {
        BuiltinType builtin;
        FunctionType function;

        ~aa();
        aa();
    } as;

    ~Type();
    Type();

    static llvm::Type* toLLVMType(Type *ty, llvm::LLVMContext &con);
};
