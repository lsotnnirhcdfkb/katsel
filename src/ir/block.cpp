#include "ir/block.h"
#include "ir/instruction.h"
#include "message/reportAbort.h"

IR::Block::Block(std::string name, size_t num): name(name), num(num) {}

IR::Instrs::Instruction* IR::Block::add(std::unique_ptr<IR::Instrs::Instruction> instr) {
    IR::Instrs::Instruction *raw = instr.get();
    instructions.push_back(std::move(instr));
    return raw;
}

void IR::Block::branch(std::unique_ptr<IR::Instrs::Br> br) {
    if (this->br)
        reportAbortNoh("Block::branch called multiple times");

    this->br = std::move(br);
}
