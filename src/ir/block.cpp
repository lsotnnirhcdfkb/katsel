#include "ir/block.h"
#include "message/errors.h"

void Block::add(std::unique_ptr<Instruction> instr)
{
    instructions.push_back(std::move(instr));
}

void Block::branch(std::unique_ptr<Branch> br)
{
    if (this->br)
        reportAbortNoh("Block::branch called multiple time");

    this->br = std::move(br);
}
