#include "ir/block.h"

void Block::add(std::unique_ptr<Instruction> instr)
{
    instructions.push_back(std::move(instr));
}
