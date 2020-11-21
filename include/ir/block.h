#pragma once

#include <vector>
#include <memory>

#include "ir/instruction.h"

class Block
{
public:
    void add(std::unique_ptr<Instruction> instr);
    void branch(std::unique_ptr<Branch> br);

private:
    std::vector<std::unique_ptr<Instruction>> instructions;
    std::unique_ptr<Branch> br;
};
