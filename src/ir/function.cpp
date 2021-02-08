#include "ir/value.h"
#include "ir/block.h"
#include "ir/type.h"
#include "ir/instruction.h"
#include "ir/function.h"
#include "utils/format.h"
#include "ast/ast.h"

IR::Function::Function(NNPtr<IR::FunctionType> ty, std::string name, NNPtr<ASTNS::FunctionDecl> def_ast):
    register_id(0),
    ret_reg(add_register(*ty->ret, *def_ast)),
    ty(ty),
    name(name),
    prototypeonly(false),
    _def_ast(def_ast),
    block_i(0) {
    for (NNPtr<IR::Type const> pty : ty->paramtys) {
        param_regs.push_back(add_register(*pty, *def_ast));
        // TODO: param def ast should not be the function decl ast
    }
}

void IR::Function::add(std::unique_ptr<IR::Block> block) {
    if (prototypeonly)
        blocks.push_back(std::move(block));
    else
        report_abort_noh("push block on prototypeonly");
}

ASTNS::AST const &IR::Function::def_ast() const {
    return *_def_ast;
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
IR::Register& IR::Function::add_register(IR::Type const &ty, ASTNS::AST const &def_ast) {
    auto reg = std::make_unique<Register>(ty, def_ast, register_id++);
    auto &reg_raw = *reg;
    registers.push_back(std::move(reg));

    return reg_raw;
}
