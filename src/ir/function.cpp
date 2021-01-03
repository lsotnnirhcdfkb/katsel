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

std::string IR::Function::stringify() const {
    std::stringstream ss;
    ss << "fun " << name;
    return ss.str();
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
void IR::Function::definition(llvm::raw_ostream &os) const {
    if (prototypeonly) {
        os << format("%: % (proto);\n", name, ty->name());
        return;
    }
    os << format("%: % {\n", name, ty->name());
    for (std::unique_ptr<Block> const &b : blocks)
        b->definition(os);
    os << "}\n";
}
void IR::Function::cfgDot(llvm::raw_ostream &os) const {
    os << "    subgraph cluster_fun_" << name << " {\n";
    os << "        graph [label=\"" << stringify() << "\"]\n";
    for (std::unique_ptr<Block> const &b : blocks)
        b->cfgDot(os);
    os << "    }\n";
}
