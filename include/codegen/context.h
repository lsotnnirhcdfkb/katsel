#pragma once

#include "codegen/codegen.h"
#include <cstdint>

#include "ir/type.h"
#include "ir/value.h"

class CodeGen::Context {
public:
    Context(File const &file, CodeGen &cg);

    IR::FloatType* getFloatType(int size);
    IR::IntType* getIntType(int size, bool isSigned);
    IR::GenericFloatType* getGenericFloatType();
    IR::GenericIntType* getGenericIntType();
    IR::CharType* getCharType();
    IR::BoolType* getBoolType();
    IR::FunctionType* getFunctionType(IR::Type *ret, std::vector<IR::Type*> paramtys);
    IR::VoidType* getVoidType();
    IR::PointerType* getPointerType(IR::Type *ty);

    IR::ConstFloat* getConstFloat(IR::FloatType *ty, double value);
    IR::ConstInt* getConstInt(IR::IntType *ty, uint64_t value);
    IR::ConstFloat* getConstFloat(IR::GenericFloatType *ty, double value);
    IR::ConstInt* getConstInt(IR::GenericIntType *ty, uint64_t value);
    IR::ConstChar* getConstChar(uint8_t value);
    IR::ConstBool* getConstBool(bool value);
    IR::Void* getVoid();

private:
    CodeGen &cg;

    std::vector<std::unique_ptr<IR::Type>> types;
    std::vector<std::unique_ptr<IR::Value>> constants;
    IR::Void voidValue;
};
