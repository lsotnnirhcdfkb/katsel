#include "ir/value.h"
#include "message/errors.h"

Register::Register(int index, Type *type, ASTNS::AST *ast, bool temp): temp(temp), index(index), ty(type), _ast(ast) {}

std::string Register::stringify() const
{
    return concatMsg("#", index);
}
ASTNS::AST* Register::ast() const
{
    return _ast;
}
Type* Register::type() const
{
    return ty;
}
bool Register::assignable() const
{
    return !temp;
}

void Register::definition(std::ostream &os) const
{
    os << "    " << ty->stringify() << " #" << index << std::endl;;
}

ConstInt::ConstInt(BuiltinType *ty, ASTNS::AST *ast, int val): val(val), ty(ty), _ast(ast) {}

ASTNS::AST* ConstInt::ast() const
{
    return _ast;
}
std::string ConstInt::stringify() const
{
    return std::to_string(val);
}

Type* ConstInt::type() const
{
    return ty;
}
bool ConstInt::assignable() const
{
    return false;
}
