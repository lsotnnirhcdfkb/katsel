#pragma once

#include "typing/type.h"
#include "llvm/IR/Value.h"
#include <cstddef>

namespace ASTNS {class Expr;}

struct Value
{
    Type *type;
    llvm::Value *val;
    ASTNS::Expr *ast; // won't be invalidated because values are only ever created during codegen, which is just visiting

    inline Value(Type *t, llvm::Value *v, ASTNS::Expr *ast): type(t), val(v), ast(ast) {}
    inline Value(): type(nullptr), val(nullptr), ast(nullptr) {}
};

struct Local
{
    size_t scopenum;
    Value v;
    std::string name;
};
