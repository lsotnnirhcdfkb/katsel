#pragma once

#include <cstdint>

#include "codegen/codegen.h"
#include "ir/type.h"
#include "ir/value.h"

class CodeGen::Context {
public:
    Context(File const &file, CodeGen &cg);

    NNPtr<IR::FloatType> get_float_type(int size);
    NNPtr<IR::IntType> get_int_type(int size, bool is_signed);
    NNPtr<IR::GenericFloatType> get_generic_float_type();
    NNPtr<IR::GenericIntType> get_generic_int_type();
    NNPtr<IR::CharType> get_char_type();
    NNPtr<IR::BoolType> get_bool_type();
    NNPtr<IR::FunctionType> get_function_type(NNPtr<IR::Type> ret, std::vector<NNPtr<IR::Type>> paramtys);
    NNPtr<IR::VoidType> get_void_type();
    NNPtr<IR::PointerType> get_pointer_type(bool mut, NNPtr<IR::Type> ty);

    NNPtr<IR::ConstFloat> get_const_float(NNPtr<IR::FloatType> ty, double value);
    NNPtr<IR::ConstInt> get_const_int(NNPtr<IR::IntType> ty, uint64_t value);
    NNPtr<IR::ConstFloat> get_const_float(NNPtr<IR::GenericFloatType> ty, double value);
    NNPtr<IR::ConstInt> get_const_int(NNPtr<IR::GenericIntType> ty, uint64_t value);
    NNPtr<IR::ConstChar> get_const_char(uint8_t value);
    NNPtr<IR::ConstBool> get_const_bool(bool value);
    NNPtr<IR::Void> get_void();

private:
    CodeGen &cg;

    std::vector<std::unique_ptr<IR::Type>> types;
    std::vector<std::unique_ptr<IR::Value>> constants;
    IR::Void void_value;
};
