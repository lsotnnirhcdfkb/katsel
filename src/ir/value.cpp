#include "ir/value.h"
#include "message/errors.h"

IR::Register::Register(int index, IR::Type *type, ASTNS::AST *defAST, bool temp): _defAST(defAST), index(index), ty(type), temp(temp) {}

std::string IR::Register::stringify() const
{
    return concatMsg("%", index);
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

IR::ConstInt::ConstInt(BuiltinType *ty, int val): val(val), ty(ty) {}

std::string IR::ConstInt::stringify() const
{
    return std::to_string(val);
}

IR::Type* IR::ConstInt::type() const
{
    return ty;
}
