#pragma once

#include <vector>
#include <memory>

#include "ir/instruction.h"

class Block
{
public:
    void add(std::unique_ptr<Instruction> instr);

private:
    std::vector<std::unique_ptr<Instruction>> instructions;
};
