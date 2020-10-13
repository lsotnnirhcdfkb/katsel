#pragma once

#include "typing/type.h"
#include "llvm/IR/Value.h"

struct Value
{
    Type *t;
    llvm::Value *val;
};
