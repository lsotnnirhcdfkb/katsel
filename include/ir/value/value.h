#pragma once

#include "ir/typing/type.h"
#include "parse/ast.h"

struct Value
{
    Type *type;
    ASTNS::AST *ast; // won't be invalidated because values are only ever created during codegen, which is just visiting

    Value(Type *t, ASTNS::AST *ast);
    Value();
};

struct Local
{
    size_t scopenum;
    Value v;
    std::string name;
};
