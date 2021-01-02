#pragma once

#include "codegen/codegen.h"
#include <cstdint>

#include "ir/type.h"
#include "ir/value.h"

class CodeGen::Context {
public:
    Context(File const &file);

    IR::FloatType* getFloatType(int size);
    IR::IntType* getIntType(int size, bool isSigned);
    IR::GenericFloatType* getGenericFloatType();
    IR::GenericIntType* getGenericIntType();
    IR::CharType* getCharType();
    IR::BoolType* getBoolType();
    IR::FunctionType* getFunctionType(IR::Type *ret, std::vector<IR::Type*> paramtys);
    IR::VoidType* getVoidType();
    IR::PointerType* getPointerType(IR::Type *ty);

    IR::Value* getGlobal(std::string const &name);
    void addGlobal(std::string const &name, IR::Value *v);

    IR::Type* getType(std::string const &name);
    void addType(std::string const &name, IR::Type *v);

    IR::ConstFloat* getConstFloat(IR::FloatType *ty, double value);
    IR::ConstInt* getConstInt(IR::IntType *ty, uint64_t value);
    IR::ConstFloat* getConstFloat(IR::GenericFloatType *ty, double value);
    IR::ConstInt* getConstInt(IR::GenericIntType *ty, uint64_t value);
    IR::ConstChar* getConstChar(uint8_t value);
    IR::ConstBool* getConstBool(bool value);
    IR::Void* getVoid();

    std::unique_ptr<ASTNS::ImplicitDecl> implicitDeclAST;

private:
    std::vector<std::unique_ptr<IR::Type>> types;
    std::vector<std::unique_ptr<IR::Value>> constants;
    IR::Void voidValue;
    std::map<std::string, IR::Value*> globalSymbolTable;
    std::map<std::string, IR::Type*> typeSymbolTable;
};
