#include "ir/value.h"
#include "ir/block.h"
#include "ir/type.h"
#include "ir/instruction.h"
#include "ir/function.h"
#include "utils/format.h"
#include "ast/ast.h"

IR::Function::Function(NNPtr<IR::FunctionType> ty, std::string name, NNPtr<ASTNS::FunctionDecl> def_ast): ty(ty), name(name), prototypeonly(false), curindex(0), _def_ast(def_ast), blocki(0) {}

void IR::Function::add(std::unique_ptr<IR::Block> block) {
    if (prototypeonly)
        blocks.push_back(std::move(block));
    else
        report_abort_noh("push block on prototypeonly");
}

NNPtr<ASTNS::AST> IR::Function::def_ast() const {
    return _def_ast;
}

NNPtr<IR::Type> IR::Function::type() const {
    return ty;
}

NNPtr<IR::Block> IR::Function::add_block(std::string name) {
    std::unique_ptr<Block> block = std::make_unique<Block>(this, name, blocki++);
    NNPtr<Block> blockraw = block.get();
    blocks.push_back(std::move(block));

    return blockraw;
}
