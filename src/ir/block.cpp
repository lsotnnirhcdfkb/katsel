#include "ir/block.h"
#include "ir/instruction.h"
#include "message/report_abort.h"

IR::Block::Block(NNPtr<IR::Function> fun, std::string const &name, size_t num): name(name), num(num), fun(fun), instr_index(0) {}

void IR::Block::branch(std::unique_ptr<IR::Br> br) {
    if (this->br)
        report_abort_noh("Block::branch called multiple times");

    this->br = std::move(br);
}

