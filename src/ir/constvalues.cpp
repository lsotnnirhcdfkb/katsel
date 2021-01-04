#include "ir/value.h"
#include "ir/type.h"
#include "utils/format.h"
#include "ir/instruction.h"

IR::ConstInt::ConstInt(IntType *ty, uint64_t val): val(val), concreteTy(ty), isGeneric(false) {}
IR::ConstInt::ConstInt(GenericIntType *ty, uint64_t val): val(val), genericTy(ty), isGeneric(true) {}
IR::Type* IR::ConstInt::type() const {
    return isGeneric ? (Type*) genericTy : (Type*) concreteTy;
}
IR::ConstFloat::ConstFloat(FloatType *ty, double val): val(val), concreteTy(ty), isGeneric(false) {}
IR::ConstFloat::ConstFloat(GenericFloatType *ty, double val): val(val), genericTy(ty), isGeneric(true) {}
IR::Type* IR::ConstFloat::type() const {
    return isGeneric ? (Type*) genericTy : (Type*) concreteTy;
}
IR::ConstBool::ConstBool(BoolType *ty, bool val): val(val), ty(ty) {}
IR::Type* IR::ConstBool::type() const {
    return ty;
}
IR::ConstChar::ConstChar(CharType *ty, uint8_t val): val(val), ty(ty) {}
IR::Type* IR::ConstChar::type() const {
    return ty;
}
IR::Void::Void(VoidType *ty): ty(ty) {}
IR::Type* IR::Void::type() const {
    return ty;
}

// again, do all the accept methods here, even though it doesn't really fit in with the file structure
#define ACCEPT(cl, name) \
void IR::cl::value_accept(IR::ValueVisitor *v) { v->value_visit##name(this); }
IR_VALUE_LIST(ACCEPT)
#undef ACCEPT
