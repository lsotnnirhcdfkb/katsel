#pragma once

#include <memory>
#include <map>
#include <vector>

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"

#include "typing/type.h"
#include "value/value.h"

class CodeGenContext
{
public:
    inline CodeGenContext(std::string const &name): builder(context), mod(std::make_unique<llvm::Module>(name, context)) {}

    llvm::LLVMContext context;
    llvm::IRBuilder<> builder;
    std::unique_ptr<llvm::Module> mod;
    std::map<std::string, Value> globalSymbolTable;

    std::vector<Local> locals;
    size_t curScope = 1;

    Type* getBuiltinType(BuiltinType::Builtins ty);
    Type* getFunctionType(Type *ret, std::vector<Type*> paramtys);
    Type* getVoidType();

    llvm::AllocaInst* createEntryAlloca(llvm::Function *f, llvm::Type *type, std::string const &name);
    void addLocal(std::string const &name, Type *type, llvm::AllocaInst *alloca);
    Local* findLocal(std::string const &name);
    Value findValue(std::string const &name);
    Value getGlobal(std::string const &name);

    void incScope();
    void decScope();

    Value curFunc;

private:
    std::vector<std::unique_ptr<Type>> types;
};
