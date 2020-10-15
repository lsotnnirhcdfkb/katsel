#pragma once

#include <memory>
#include <map>

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"

#include "typing/type.h"
#include "value/value.h"

class CodeGenContext
{
public:
    inline CodeGenContext(): builder(context) {}

    llvm::LLVMContext context;
    llvm::IRBuilder<> builder;
    std::unique_ptr<llvm::Module> mod;
    std::map<std::string, Value> globalSymbolTable;

    Type* getBuiltinType(BuiltinType ty);
    Type* getFunctionType(Type *ret, std::vector<Type*> paramtys);

private:
    std::vector<std::unique_ptr<Type>> types;
};
