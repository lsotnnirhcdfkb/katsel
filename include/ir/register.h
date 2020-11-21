#pragma once

#include "ir/type.h"
#include <string>

class Register
{
public:
    Register(int index, Type *type);
    std::string stringify();

private:
    int index;

    Type *type;
};
