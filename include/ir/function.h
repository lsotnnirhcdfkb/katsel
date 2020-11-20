#pragma once

#include "ir/value/value.h"
#include "ir/typing/type.h"

#include <string>
#include <vector>

class Function
{
public:
    Function(FunctionType *ty, std::string name);

private:
    std::vector<int> instructions; // TODO: make instruction class and replace dummy int type with actual instruction class
    FunctionType *ty;
    std::string name;
};
