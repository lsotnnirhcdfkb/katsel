#include "ir/value.h"
#include "message/errors.h"

Register::Register(int index, Type *type, ASTNS::AST *ast): index(index), ty(type), _ast(ast) {}

std::string Register::stringify() const
{
    return std::string(0, '#') + std::to_string(index);
}
ASTNS::AST* Register::ast() const
{
    return _ast;
}
Type* Register::type() const
{
    return ty;
}

Function::Function(FunctionType *ty, std::string name, ASTNS::Function *ast): ty(ty), name(name), _ast(ast) {}

void Function::add(std::unique_ptr<Block> block)
{
    blocks.push_back(std::move(block));
}

std::string Function::stringify() const
{
    return concatMsg("<function \"", name, "\">");
}
ASTNS::AST* Function::ast() const
{
    return _ast;
}

Type* Function::type() const
{
    return ty;
}
