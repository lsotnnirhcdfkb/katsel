#pragma once

#include "ir/type.h"
#include <string>
#include "ast/ast.h"

struct Param
{
    Type *ty;
    std::string name;
    ASTNS::Param *ast;
};
