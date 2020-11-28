#include "ir/block.h"
#include "message/errors.h"
#include "ir/printer.h"
#include "ir/cfgdotter.h"

IR::Block::Block(std::string name, size_t num): name(name), num(num) {}

void IR::Block::add(std::unique_ptr<IR::Instrs::Instruction> instr)
{
    instructions.push_back(std::move(instr));
}

void IR::Block::branch(std::unique_ptr<IR::Instrs::Br> br)
{
    if (this->br)
        reportAbortNoh("Block::branch called multiple times");

    this->br = std::move(br);
}

void IR::Block::stringify(llvm::raw_ostream &os)
{
    os << name << "(" << num << ")";
}

void IR::Block::definition(llvm::raw_ostream &os)
{
    IR::Printer p (os);
    os << "    " << name << "(" << num << "): {\n";
    for (std::unique_ptr<Instrs::Instruction> const &i : instructions)
    {
        os << "        ";
        i->accept(&p);
        os << "\n";
    }
    os << "    ----\n";
    if (br)
    {
        os << "        ";
        br->accept(&p);
        os << "\n";
    }
    os << "    }\n";
}
void IR::Block::cfgDot(llvm::raw_ostream &os)
{
    IR::Printer p (os);
    IR::CFGDotter c (os);

    os << "        block" << this << " [shape=record,label=\"-- ";
    stringify(os);
    os << " --\\l";
    for (std::unique_ptr<Instrs::Instruction> const &i : instructions)
    {
        i->accept(&p);
        os << "\\l";
    }
    os << "\"]\n";

    if (br)
    {
        os << "        block" << this << " -> branch" << br.get() << "\n";
        os << "        branch" << br.get() << " [shape=record, label=\"";
        br->accept(&p);
        os << "\"]\n";

        br->accept(&c);
    }
}
