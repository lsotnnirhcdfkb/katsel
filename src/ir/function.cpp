#include "ir/value.h"
#include "ir/type.h"
#include "ir/instruction.h"
#include "utils/format.h"

IR::Function::Function(IR::FunctionType *ty, std::string name, ASTNS::FunctionDecl *defAST): ty(ty), name(name), prototypeonly(false), _defAST(defAST), blocki(0) {}

void IR::Function::add(std::unique_ptr<IR::Block> block) {
    if (prototypeonly)
        blocks.push_back(std::move(block));
    else
        reportAbortNoh("push block on prototypeonly");
}

ASTNS::FunctionDecl* IR::Function::defAST() const {
    return _defAST;
}

IR::Type* IR::Function::type() const {
    return ty;
}

IR::Block* IR::Function::addBlock(std::string name) {
    std::unique_ptr<Block> block = std::make_unique<Block>(name, blocki++);
    Block *blockraw = block.get();
    blocks.push_back(std::move(block));

    return blockraw;
}
