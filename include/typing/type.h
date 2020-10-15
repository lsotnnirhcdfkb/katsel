#pragma once

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
    DOUBLE
};

struct Type;

class FunctionType
{
public:
    Type *ret;
    std::vector<Type*> paramtys;

    inline ~FunctionType() {}
    inline FunctionType() {}
};

struct Type
{
    TypeType typetype;

    union aa // need destructor because FunctionType has non-trivial destructor
    {
        BuiltinType builtin;
        FunctionType function;

        inline ~aa() {}
        inline aa() {}
    } as;

    inline ~Type() {}
    inline Type() {}
};
