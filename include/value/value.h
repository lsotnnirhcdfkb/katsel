#pragma once

#include "typing/type.h"
#include "llvm/IR/Value.h"
#include <cstddef>

struct Value
{
    Type *t;
    llvm::Value *val;

    inline Value(Type *t, llvm::Value *v): t(t), val(v) {}
    inline Value(): t(nullptr), val(nullptr) {}
};

struct Local
{
    size_t scopenum;
    Value v;
}
