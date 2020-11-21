#pragma once

#include <vector>
#include <memory>
#include <string>

#include "ir/instruction.h"

class Block
{
public:
    Block(std::string name, size_t num);
    void add(std::unique_ptr<Instrs::Instruction> instr);
    void branch(std::unique_ptr<Instrs::Br> br);

private:
    std::string name;
    size_t num;
    std::vector<std::unique_ptr<Instrs::Instruction>> instructions;
    std::unique_ptr<Instrs::Br> br;
};
