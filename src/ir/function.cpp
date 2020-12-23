#include "ir/value.h"
#include "ir/type.h"
#include "ir/instruction.h"
#include "utils/format.h"

IR::Function::Function(IR::FunctionType *ty, std::string name, ASTNS::FunctionDecl *defAST): ty(ty), name(name), prototypeonly(false), _defAST(defAST), blocki(0), regi(0), tempregi(0) {}

void IR::Function::add(std::unique_ptr<IR::Block> block)
{
    if (prototypeonly)
        blocks.push_back(std::move(block));
    else
        reportAbortNoh("push block on prototypeonly");
}

std::string IR::Function::stringify() const
{
    std::stringstream ss;
    ss << "fun " << name;
    return ss.str();
}
ASTNS::FunctionDecl* IR::Function::defAST() const
{
    return _defAST;
}

IR::Type* IR::Function::type() const
{
    return ty;
}

IR::Block* IR::Function::addBlock(std::string name)
{
    std::unique_ptr<Block> block = std::make_unique<Block>(name, blocki++);
    Block *blockraw = block.get();
    blocks.push_back(std::move(block));

    return blockraw;
}
IR::Register* IR::Function::addRegister(Type *type, ASTNS::AST *ast)
{
    std::unique_ptr<Register> reg = std::make_unique<Register>(regi++, type, ast);
    Register *regraw = reg.get();
    registers.push_back(std::move(reg));
    return regraw;
}
IR::TempRegister* IR::Function::addTempRegister(Type *type)
{
    std::unique_ptr<TempRegister> reg = std::make_unique<TempRegister>(tempregi++, type);
    TempRegister *regraw = reg.get();
    tempregisters.push_back(std::move(reg));
    return regraw;
}

void IR::Function::definition(llvm::raw_ostream &os) const
{
    if (prototypeonly)
    {
        os << format("%: %;\n", name, ty->stringify());
        return;
    }
    os << format("%: % {\n", name, ty->stringify());
    for (std::unique_ptr<Register> const &r : registers)
        r->definition(os);

    for (std::unique_ptr<Block> const &b : blocks)
        b->definition(os);
    os << "}\n";
}
void IR::Function::cfgDot(llvm::raw_ostream &os) const
{
    os << "    subgraph cluster_fun_" << name << " {\n";
    os << "        graph [label=\"" << stringify() << "\"]\n";
    for (std::unique_ptr<Block> const &b : blocks)
        b->cfgDot(os);
    os << "    }\n";
}
