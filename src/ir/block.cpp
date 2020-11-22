#include "ir/block.h"
#include "message/errors.h"

Block::Block(std::string name, size_t num): name(name), num(num) {}

void Block::add(std::unique_ptr<Instrs::Instruction> instr)
{
    instructions.push_back(std::move(instr));
}

void Block::branch(std::unique_ptr<Instrs::Br> br)
{
    if (this->br)
        reportAbortNoh("Block::branch called multiple times");

    this->br = std::move(br);
}

void Block::stringify(std::ostream &os)
{
    os << name << "(" << num << ")";
}

void Block::definition(std::ostream &os)
{
    os << "    " << name << "(" << num << "): {\n";
    for (std::unique_ptr<Instrs::Instruction> const &i : instructions)
    {
        os << "        ";
        i->stringify(os);
    }
    os << "    ----\n";
    if (br)
    {
        os << "        ";
        br->stringify(os);
    }
    os << "    }\n";
}
