#include "ir/value.h"
#include "ir/type.h"
#include "utils/format.h"
#include "ir/instruction.h"

IR::ConstInt::ConstInt(NNPtr<IntType>        ty, uint64_t val): val(val), ty(ty), isGeneric(false) {}
IR::ConstInt::ConstInt(NNPtr<GenericIntType> ty, uint64_t val): val(val), ty(ty), isGeneric(true) {}
NNPtr<IR::Type> IR::ConstInt::type() const {
    return isGeneric ? (NNPtr<Type>) std::get<NNPtr<GenericIntType>>(ty) : (NNPtr<Type>) std::get<NNPtr<IntType>>(ty);
}
IR::ConstFloat::ConstFloat(NNPtr<FloatType>        ty, double val): val(val), ty(ty), isGeneric(false) {}
IR::ConstFloat::ConstFloat(NNPtr<GenericFloatType> ty, double val): val(val), ty(ty), isGeneric(true) {}
NNPtr<IR::Type> IR::ConstFloat::type() const {
    return isGeneric ? (NNPtr<Type>) std::get<NNPtr<GenericFloatType>>(ty) : (NNPtr<Type>) std::get<NNPtr<FloatType>>(ty);
}
IR::ConstBool::ConstBool(NNPtr<BoolType> ty, bool val): val(val), ty(ty) {}
NNPtr<IR::Type> IR::ConstBool::type() const {
    return ty;
}
IR::ConstChar::ConstChar(NNPtr<CharType> ty, uint8_t val): val(val), ty(ty) {}
NNPtr<IR::Type> IR::ConstChar::type() const {
    return ty;
}
IR::Void::Void(NNPtr<VoidType> ty): ty(ty) {}
NNPtr<IR::Type> IR::Void::type() const {
    return ty;
}

// again, do all the accept methods here, even though it doesn't really fit in with the file structure
#define ACCEPT(cl, name) \
void IR::cl::value_accept(NNPtr<IR::ValueVisitor> v) { v->value_visit##name(this); }
IR_VALUE_LIST(ACCEPT)
#undef ACCEPT
