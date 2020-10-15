#pragma once

enum TypeType
{
    BUILTIN,
    META,
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

enum class MetaType
{
    FUNCTION,
};

struct Type
{
    TypeType typetype;

    union
    {
        BuiltinType builtin;
        MetaType meta;
    } as;
};
