#include "ir/value.h"
#include "ir/type.h"
#include "ir/instruction.h"
#include "utils/format.h"
#include "ast/ast.h"

IR::Function::Function(NNPtr<IR::FunctionType> ty, std::string name, NNPtr<ASTNS::FunctionDecl> defAST): ty(ty), name(name), prototypeonly(false), curindex(0), _defAST(defAST), blocki(0) {}

void IR::Function::add(std::unique_ptr<IR::Block> block) {
    if (prototypeonly)
        blocks.push_back(std::move(block));
    else
        reportAbortNoh("push block on prototypeonly");
}

NNPtr<ASTNS::AST> IR::Function::defAST() const {
    return _defAST;
}

NNPtr<IR::Type> IR::Function::type() const {
    return ty;
}

NNPtr<IR::Block> IR::Function::addBlock(std::string name) {
    std::unique_ptr<Block> block = std::make_unique<Block>(this, name, blocki++);
    NNPtr<Block> blockraw = block.get();
    blocks.push_back(std::move(block));

    return blockraw;
}
