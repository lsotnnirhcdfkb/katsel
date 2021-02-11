#include "ir/value.h"
#include "ir/function.h"
#include "ir/type.h"
#include "utils/format.h"
#include "ir/instruction.h"

IR::ConstInt::ConstInt(IntType        &ty, uint64_t val): val(val), ty(ty), is_generic(false) {}
IR::ConstInt::ConstInt(GenericIntType &ty, uint64_t val): val(val), ty(ty), is_generic(true) {}
IR::Type const &IR::ConstInt::type() const {
    return *(is_generic ? static_cast<Type*>(std::get<NNPtr<GenericIntType>>(ty).as_raw()) : static_cast<Type*>(std::get<NNPtr<IntType>>(ty).as_raw()));
}
IR::ConstFloat::ConstFloat(FloatType        &ty, double val): val(val), ty(ty), is_generic(false) {}
IR::ConstFloat::ConstFloat(GenericFloatType &ty, double val): val(val), ty(ty), is_generic(true) {}
IR::Type const &IR::ConstFloat::type() const {
    return *(is_generic ? static_cast<Type*>(std::get<NNPtr<GenericFloatType>>(ty).as_raw()) : static_cast<Type*>(std::get<NNPtr<FloatType>>(ty).as_raw()));
}
IR::ConstBool::ConstBool(BoolType &ty, bool val): val(val), ty(ty) {}
IR::Type const &IR::ConstBool::type() const {
    return *ty;
}
IR::ConstChar::ConstChar(CharType &ty, uint8_t val): val(val), ty(ty) {}
IR::Type const &IR::ConstChar::type() const {
    return *ty;
}
IR::Void::Void(VoidType &ty): ty(ty) {}
IR::Type const &IR::Void::type() const {
    return *ty;
}

// again, do all the accept methods here, even though it doesn't really fit in with the file structure
#define ACCEPT(cl) \
void IR::cl::value_accept(IR::ValueVisitor &v) const { v.value_visit(*this); }
IR_VALUE_LIST(ACCEPT)
#undef ACCEPT
