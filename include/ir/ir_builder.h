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

        NNPtr<Codegen::Context> _context;

    public:
        inline Builder(IR::Function &fun, IR::Block &reg_block, IR::Block &exit_block, IR::Instrs::Register &ret_reg, IR::Block &cur_block, Codegen::Context &context):
            _fun(fun), _register_block(reg_block), _exit_block(exit_block), _ret_reg(ret_reg), _cur_block(cur_block), _context(context) {}
        Codegen::Context &context() { return *_context; }
        inline IR::Function &fun() { return *_fun; }
        inline IR::Block &register_block() { return *_register_block; }
        inline IR::Block &exit_block() { return *_exit_block; }
        inline IR::Instrs::Register &ret_reg() { return *_ret_reg; }
        inline NNPtr<IR::Block> &cur_block() { return _cur_block; }
    };
}
