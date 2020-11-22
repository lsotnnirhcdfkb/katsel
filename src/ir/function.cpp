#include "ir/value.h"
#include "message/errors.h"

Function::Function(FunctionType *ty, std::string name, ASTNS::Function *ast): ty(ty), name(name), _ast(ast), blocki(0), regi(0) {}

void Function::add(std::unique_ptr<Block> block)
{
    blocks.push_back(std::move(block));
}

std::string Function::stringify() const
{
    std::stringstream ss;
    ss << "fun \"" << name << "\"";
    return ss.str();
}
ASTNS::AST* Function::ast() const
{
    return _ast;
}

Type* Function::type() const
{
    return ty;
}
bool Function::assignable() const
{
    return false;
}

Block* Function::addBlock(std::string name)
{
    std::unique_ptr<Block> block = std::make_unique<Block>(name, blocki++);
    Block *blockraw = block.get();
    blocks.push_back(std::move(block));

    return blockraw;
}
Register* Function::addRegister(Type *type, ASTNS::AST *ast, bool temp)
{
    std::unique_ptr<Register> reg = std::make_unique<Register>(regi++, type, ast, temp);
    Register *regraw = reg.get();
    registers.push_back(std::move(reg));
    return regraw;
}

void Function::definition(std::ostream &os) const
{
    os << "fun \"" << name << "\" " << ty->stringify() << " {\n";
    for (std::unique_ptr<Register> const &r : registers)
        r->definition(os);

    for (std::unique_ptr<Block> const &b : blocks)
        b->definition(os);
    os << "}\n";
}
void Function::cfgDot(std::ostream &os) const
{
    os << "subgraph cluster_fun_" << name << "{\n";
    for (std::unique_ptr<Block> const &b : blocks)
        b->cfgDot(os);
    os << "}\n";
}
