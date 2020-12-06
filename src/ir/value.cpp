#include "ir/value.h"
#include "message/errors.h"
#include "utils/format.h"

IR::Register::Register(int index, IR::Type *type, ASTNS::AST *defAST): _defAST(defAST), index(index), ty(type) {}

std::string IR::Register::stringify() const
{
    return format("%%%", index);
}
ASTNS::AST* IR::Register::defAST() const
{
    return _defAST;
}
IR::Type* IR::Register::type() const
{
    return ty;
}

void IR::Register::definition(llvm::raw_ostream &os) const
{
    os << "    " << ty->stringify() << " %" << index << "\n";
}

IR::TempRegister::TempRegister(int index, Type *type): index(index), ty(type) {}

std::string IR::TempRegister::stringify() const
{
    return format("$%", index);
}
IR::Type* IR::TempRegister::type() const
{
    return ty;
}

IR::ConstInt::ConstInt(BuiltinType *ty, uint64_t val): val(val), ty(ty) {}

std::string IR::ConstInt::stringify() const
{
    return std::to_string(val);
}

IR::Type* IR::ConstInt::type() const
{
    return ty;
}
