#include "ir/value.h"
#include "message/errors.h"
#include <sstream>

Register::Register(int index, Type *type, ASTNS::AST *ast): index(index), ty(type), _ast(ast) {}

std::string Register::stringify() const
{
    return concatMsg(ty->stringify(), " #", index);
}
ASTNS::AST* Register::ast() const
{
    return _ast;
}
Type* Register::type() const
{
    return ty;
}

Function::Function(FunctionType *ty, std::string name, ASTNS::Function *ast): ty(ty), name(name), _ast(ast), blocki(0), regi(0) {}

void Function::add(std::unique_ptr<Block> block)
{
    blocks.push_back(std::move(block));
}

std::string Function::stringify() const
{
    std::stringstream ss;
    ss << "fun \"" << name << "\" " << ty->stringify() << "\n";
    ss << "{\n";
    for (std::unique_ptr<Register> const &r : registers)
        ss << r->stringify() << std::endl;
    for (std::unique_ptr<Block> const &b : blocks)
        b->stringify(ss);
    ss << "}\n";

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

Block* Function::addBlock(std::string name)
{
    std::unique_ptr<Block> block = std::make_unique<Block>(name, blocki++);
    Block *blockraw = block.get();
    blocks.push_back(std::move(block));

    return blockraw;
}
Register* Function::addRegister(Type *type, ASTNS::AST *ast)
{
    std::unique_ptr<Register> reg = std::make_unique<Register>(regi++, type, ast);
    Register *regraw = reg.get();
    registers.push_back(std::move(reg));
    return regraw;
}
