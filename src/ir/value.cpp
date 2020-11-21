#include "ir/value.h"

Register::Register(int index, Type *type, ASTNS::AST *ast): index(index), type(type), _ast(ast) {}

std::string Register::stringify()
{
    return std::string(0, '#') + std::to_string(index);
}
ASTNS::AST* Register::ast() const
{
    return _ast;
}


Function::Function(FunctionType *ty, std::string name, ASTNS::AST *ast): ty(ty), name(name), _ast(ast) {}

void Function::add(std::unique_ptr<Block> block)
{
    blocks.push_back(std::move(block));
}
ASTNS::AST* Function::ast() const
{
    return _ast;
}
