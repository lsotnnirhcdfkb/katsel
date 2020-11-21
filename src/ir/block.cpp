#include "ir/block.h"
#include "message/errors.h"

void Block::add(std::unique_ptr<Instrs::Instruction> instr)
{
    instructions.push_back(std::move(instr));
}

void Block::branch(std::unique_ptr<Instrs::Br> br)
{
    if (this->br)
        reportAbortNoh("Block::branch called multiple time");

    this->br = std::move(br);
}
