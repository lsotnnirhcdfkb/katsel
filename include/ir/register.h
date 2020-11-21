#pragma once

#include "ir/type.h"

struct Register
{
    int index;
    bool temp;

    Type *type;

    Register(int index, bool temp, Type *type);
};
