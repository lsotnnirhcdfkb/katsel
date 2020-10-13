#pragma once

enum class TypeType
{
    BASICTY
};

enum class BasicType
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

union Type
{
    TypeType typetype;
    struct {
        TypeType typetype;
        BasicType type;
    } basictype;
};
