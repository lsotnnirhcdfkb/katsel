#pragma once

#include "ir/function.h"
#include "ir/block.h"
#include "ir/instructionfwd.h"
#include "codegen/context.h"

namespace IR {
    class Builder {
        NNPtr<IR::Function> _fun;
        NNPtr<IR::Block> _exit_block;

        NNPtr<IR::Block> _cur_block;

        NNPtr<Codegen::Context> _context;

    public:
        inline Builder(IR::Function &fun, IR::Block &exit_block, IR::Block &cur_block, Codegen::Context &context):
            _fun(fun), _exit_block(exit_block), _cur_block(cur_block), _context(context) {}
        Codegen::Context &context() { return *_context; }
        inline IR::Function &fun() { return *_fun; }
        inline IR::Block &exit_block() { return *_exit_block; }
        inline NNPtr<IR::Block> &cur_block() { return _cur_block; }
    };
}
