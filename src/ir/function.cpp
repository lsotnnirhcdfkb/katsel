#include "ir/function.h"

Function::Function(FunctionType *ty, std::string name): ty(ty), name(name) {}

void Function::add(std::unique_ptr<Block> block)
{
    blocks.push_back(std::move(block));
}
