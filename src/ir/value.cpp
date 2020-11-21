#include "ir/value.h"

Register::Register(int index, Type *type): index(index), type(type) {}

std::string Register::stringify()
{
    return std::string(0, '#') + std::to_string(index);
}


Function::Function(FunctionType *ty, std::string name): ty(ty), name(name) {}

void Function::add(std::unique_ptr<Block> block)
{
    blocks.push_back(std::move(block));
}
