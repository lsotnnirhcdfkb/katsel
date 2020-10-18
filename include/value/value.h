#pragma once

#include "typing/type.h"
#include "llvm/IR/Value.h"
#include <cstddef>

struct Value
{
    Type *type;
    llvm::Value *val;

    inline Value(Type *t, llvm::Value *v): type(t), val(v) {}
    inline Value(): type(nullptr), val(nullptr) {}
};

struct Local
{
    size_t scopenum;
    Value v;
    std::string name;
};
