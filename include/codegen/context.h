#pragma once

#include <cstdint>

#include "ir/type.h"
#include "ir/value.h"

namespace Codegen {
    class Context {
    public:
        Context();

        IR::FloatType& get_float_type(int size);
        IR::IntType& get_int_type(int size, bool is_signed);
        IR::GenericFloatType& get_generic_float_type();
        IR::GenericIntType& get_generic_int_type();
        IR::CharType& get_char_type();
        IR::BoolType& get_bool_type();
        IR::FunctionType& get_function_type(IR::Type const &ret, std::vector<NNPtr<IR::Type const>> paramtys);
        IR::VoidType& get_void_type();
        IR::PointerType& get_pointer_type(bool mut, IR::Type const &ty);

        IR::ConstFloat& get_const_float(IR::FloatType &ty, double value);
        IR::ConstInt& get_const_int(IR::IntType& ty, uint64_t value);
        IR::ConstFloat& get_const_float(IR::GenericFloatType& ty, double value);
        IR::ConstInt& get_const_int(IR::GenericIntType& ty, uint64_t value);
        IR::ConstChar& get_const_char(uint8_t value);
        IR::ConstBool& get_const_bool(bool value);
        IR::Void& get_void();

    private:
        std::vector<std::unique_ptr<IR::Type>> types;
        std::vector<std::unique_ptr<IR::Value>> constants;
        IR::Void void_value;
    };
}
