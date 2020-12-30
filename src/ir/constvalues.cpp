#include "ir/value.h"
#include "ir/type.h"
#include "utils/format.h"

IR::ConstInt::ConstInt(IntType *ty, uint64_t val): val(val), concreteTy(ty), isGeneric(false) {}
IR::ConstInt::ConstInt(GenericIntType *ty, uint64_t val): val(val), genericTy(ty), isGeneric(true) {}
std::string IR::ConstInt::stringify() const {
    return std::to_string(val);
}
IR::Type* IR::ConstInt::type() const {
    return isGeneric ? (Type*) genericTy : (Type*) concreteTy;
}
IR::ConstFloat::ConstFloat(FloatType *ty, double val): val(val), concreteTy(ty), isGeneric(false) {}
IR::ConstFloat::ConstFloat(GenericFloatType *ty, double val): val(val), genericTy(ty), isGeneric(true) {}
std::string IR::ConstFloat::stringify() const {
    return std::to_string(val);
}
IR::Type* IR::ConstFloat::type() const {
    return isGeneric ? (Type*) genericTy : (Type*) concreteTy;
}
IR::ConstBool::ConstBool(BoolType *ty, bool val): val(val), ty(ty) {}
std::string IR::ConstBool::stringify() const {
    return val ? "true" : "false";
}
IR::Type* IR::ConstBool::type() const {
    return ty;
}
IR::ConstChar::ConstChar(CharType *ty, uint8_t val): val(val), ty(ty) {}
std::string IR::ConstChar::stringify() const {
    return format("'%'", (char) val);
}
IR::Type* IR::ConstChar::type() const {
    return ty;
}
IR::Void::Void(VoidType *ty): ty(ty) {}
std::string IR::Void::stringify() const {
    return "void";
}
IR::Type* IR::Void::type() const {
    return ty;
}
