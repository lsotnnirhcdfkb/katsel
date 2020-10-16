#pragma once

#include "typing/type.h"
#include "llvm/IR/Value.h"

struct Value
{
    Type *t;
    llvm::Value *val;

    inline Value(Type *t, llvm::Value *v): t(t), val(v) {}
    inline Value(): t(nullptr), val(nullptr) {}
};
