#pragma once

#include <vector>
#include <memory>

#include "ir/instruction.h"

class Block
{
public:
    void add(std::unique_ptr<Instrs::Instruction> instr);
    void branch(std::unique_ptr<Instrs::Br> br);

private:
    std::vector<std::unique_ptr<Instrs::Instruction>> instructions;
    std::unique_ptr<Instrs::Br> br;
};
