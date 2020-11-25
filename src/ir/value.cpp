#include "ir/value.h"
#include "message/errors.h"

IR::Register::Register(int index, IR::Type *type, ASTNS::AST *ast, bool temp): index(index), temp(temp), ty(type), _ast(ast) {}

std::string IR::Register::stringify() const
{
    return concatMsg("%", index);
}
ASTNS::AST* IR::Register::ast() const
{
    return _ast;
}
IR::Type* IR::Register::type() const
{
    return ty;
}
bool IR::Register::assignable() const
{
    return !temp;
}

void IR::Register::definition(std::ostream &os) const
{
    os << "    " << ty->stringify() << " %" << index << std::endl;;
}

IR::ConstInt::ConstInt(BuiltinType *ty, ASTNS::AST *ast, int val): val(val), ty(ty), _ast(ast) {}

ASTNS::AST* IR::ConstInt::ast() const
{
    return _ast;
}
std::string IR::ConstInt::stringify() const
{
    return std::to_string(val);
}

IR::Type* IR::ConstInt::type() const
{
    return ty;
}
bool IR::ConstInt::assignable() const
{
    return false;
}
