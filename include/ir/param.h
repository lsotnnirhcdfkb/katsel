#pragma once

#include "ir/type.h"
#include <string>
#include "parse/ast.h"

struct Param
{
    Type *ty;
    std::string name;
    ASTNS::Param *ast;
};
