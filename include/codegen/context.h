#pragma once

#include <cstdint>

#include "codegen/codegen.h"
#include "ir/type.h"
#include "ir/value.h"

class CodeGen::Context {
public:
    Context(File const &file, CodeGen &cg);

    NNPtr<IR::FloatType> getFloatType(int size);
    NNPtr<IR::IntType> getIntType(int size, bool isSigned);
    NNPtr<IR::GenericFloatType> getGenericFloatType();
    NNPtr<IR::GenericIntType> getGenericIntType();
    NNPtr<IR::CharType> getCharType();
    NNPtr<IR::BoolType> getBoolType();
    NNPtr<IR::FunctionType> getFunctionType(NNPtr<IR::Type> ret, std::vector<NNPtr<IR::Type>> paramtys);
    NNPtr<IR::VoidType> getVoidType();
    NNPtr<IR::PointerType> getPointerType(bool mut, NNPtr<IR::Type> ty);

    NNPtr<IR::ConstFloat> getConstFloat(NNPtr<IR::FloatType> ty, double value);
    NNPtr<IR::ConstInt> getConstInt(NNPtr<IR::IntType> ty, uint64_t value);
    NNPtr<IR::ConstFloat> getConstFloat(NNPtr<IR::GenericFloatType> ty, double value);
    NNPtr<IR::ConstInt> getConstInt(NNPtr<IR::GenericIntType> ty, uint64_t value);
    NNPtr<IR::ConstChar> getConstChar(uint8_t value);
    NNPtr<IR::ConstBool> getConstBool(bool value);
    NNPtr<IR::Void> getVoid();

private:
    CodeGen &cg;

    std::vector<std::unique_ptr<IR::Type>> types;
    std::vector<std::unique_ptr<IR::Value>> constants;
    IR::Void voidValue;
};
