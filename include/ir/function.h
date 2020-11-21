#pragma once

#include "ir/type.h"
#include "ir/block.h"

#include <string>
#include <vector>
#include <memory>

class Function
{
public:
    Function(FunctionType *ty, std::string name);

    void add(std::unique_ptr<Block> block);

private:
    std::vector<std::unique_ptr<Block>> blocks;
    FunctionType *ty;
    std::string name;
};
