#include "ir/value.h"
#include "message/errors.h"
#include <sstream>

Register::Register(int index, Type *type, ASTNS::AST *ast, bool temp): temp(temp), index(index), ty(type), _ast(ast) {}

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
bool Register::assignable() const
{
    return !temp;
}

AliasVal::AliasVal(Value *val, ASTNS::AST *ast): v(val), _ast(ast) {}
std::string AliasVal::stringify() const
{
    return concatMsg("alias to ", v->stringify());
}
ASTNS::AST* AliasVal::ast() const
{
    return _ast;
}
Type* AliasVal::type() const
{
    return v->type();
}
bool AliasVal::assignable() const
{
    return v->assignable();
}
Value* AliasVal::get() const
{
    return v;
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
AliasVal* Function::addAlias(Value *val, ASTNS::AST *ast)
{
    std::unique_ptr<AliasVal> aval = std::make_unique<AliasVal>(val, ast);
    AliasVal *avalraw = aval.get();
    aliases.push_back(std::move(aval));
    return avalraw;
}

ConstInt::ConstInt(BuiltinType *ty, ASTNS::AST *ast, int val): val(val), ty(ty), _ast(ast) {}

ASTNS::AST* ConstInt::ast() const
{
    return _ast;
}
std::string ConstInt::stringify() const
{
    return std::to_string(val);
}

Type* ConstInt::type() const
{
    return ty;
}
bool ConstInt::assignable() const
{
    return false;
}
