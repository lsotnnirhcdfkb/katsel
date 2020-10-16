#pragma once

#include <memory>
#include <map>
#include <stack>

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"

#include "typing/type.h"
#include "value/value.h"

class CodeGenContext
{
public:
    inline CodeGenContext(): builder(context), mod(std::make_unique<llvm::Module>("thingy", context)) {}

    llvm::LLVMContext context;
    llvm::IRBuilder<> builder;
    std::unique_ptr<llvm::Module> mod;
    std::map<std::string, Value> globalSymbolTable;

    std::stack<Local> locals;
    size_t curScope;

    Type* getBuiltinType(BuiltinType::Builtins ty);
    Type* getFunctionType(Type *ret, std::vector<Type*> paramtys);

    llvm::AllocaInst* createEntryAlloca(llvm::Function *f, llvm::Type *type, std::string const &name);
    void addLocal(std::string const &name, Type *type, llvm::AllocaInst *alloca);

    void incScope();
    void decScope();

private:
    std::vector<std::unique_ptr<Type>> types;
};
