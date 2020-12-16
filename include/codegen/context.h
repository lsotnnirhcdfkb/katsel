#pragma once

#include "codegen/codegen.h"

#include "ir/type.h"
#include "ir/value.h"

class CodeGen::Context
{
public:
    Context();

    IR::BuiltinType* getBuiltinType(IR::BuiltinType::Builtins ty);
    IR::FunctionType* getFunctionType(IR::Type *ret, std::vector<IR::Type*> paramtys);
    IR::VoidType* getVoidType();

    IR::Value* getGlobal(std::string const &name);
    void addGlobal(std::string const &name, IR::Value *v);

    IR::ConstInt* getConstInt(IR::BuiltinType *ty, int val);
    IR::Void* getVoidValue();

private:
    std::vector<std::unique_ptr<IR::Type>> types;
    std::vector<std::unique_ptr<IR::ConstInt>> constants;
    IR::Void voidValue;
    std::map<std::string, IR::Value*> globalSymbolTable;
};
