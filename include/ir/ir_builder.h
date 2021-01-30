#pragma once

#include "ir/function.h"
#include "ir/block.h"
#include "ir/instructionfwd.h"
#include "codegen/context.h"

namespace IR {
    class Builder {
        NNPtr<IR::Function> _fun;
        NNPtr<IR::Block> _register_block;
        NNPtr<IR::Block> _exit_block;
        NNPtr<IR::Instrs::Register> _ret_reg;

        NNPtr<IR::Block> _cur_block;

        NNPtr<CodeGen::Context> _context;

    public:
        CodeGen::Context &context() { return *_context; }
        inline IR::Function &fun() { return *_fun; }
        inline IR::Block &register_block() { return *_register_block; }
        inline IR::Block &exit_block() { return *_exit_block; }
        inline IR::Instrs::Register &ret_reg() { return *_ret_reg; }
        inline NNPtr<IR::Block> &cur_block() { return _cur_block; }
    };
}
