#include "ir/value.h"
#include "ir/block.h"
#include "ir/type.h"
#include "ir/instruction.h"
#include "ir/function.h"
#include "utils/format.h"
#include "ast/ast.h"

IR::Function::Function(NNPtr<IR::FunctionType> ty, std::string name, Span const &def_span, std::vector<IR::Function::Param> const &params):
    register_id(0),
    ret_reg(add_register(*ty->ret, def_span, true)),
    ty(ty),
    name(name),
    prototypeonly(false),
    _def_span(def_span),
    block_i(0) {
    for (IR::Function::Param const &param : params) {
        param_regs.push_back(add_register(*param.ty, *param.ast, param.mut));
    }
}

void IR::Function::add(std::unique_ptr<IR::Block> block) {
    if (prototypeonly)
        blocks.push_back(std::move(block));
    else
        report_abort_noh("push block on prototypeonly");
}

Span const &IR::Function::def_span() const {
    return _def_span;
}

IR::Type const &IR::Function::type() const {
    return *ty;
}

IR::Block& IR::Function::add_block(std::string name) {
    std::unique_ptr<Block> block = std::make_unique<Block>(this, name, block_i++);
    Block &blockraw = *block;
    blocks.push_back(std::move(block));

    return blockraw;
}
IR::Register& IR::Function::add_register(IR::Type const &ty, Span const &def_span, bool mut) {
    auto reg = std::make_unique<Register>(ty, def_span, mut, register_id++);
    auto &reg_raw = *reg;
    registers.push_back(std::move(reg));

    return reg_raw;
}
