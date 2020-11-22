#pragma once

#include "ir/type.h"
#include "ir/block.h"
#include <string>
#include <cstddef>
#include <vector>

class Value
{
public:
    virtual ~Value() {};
    virtual ASTNS::AST* ast() const = 0;
    virtual std::string stringify() const = 0;

    virtual bool assignable() const = 0;

    virtual Type* type() const = 0;
};

class Register : public Value
{
public:
    Register(int index, Type *type, ASTNS::AST *ast, bool temp=true);

    ASTNS::AST* ast() const override;
    std::string stringify() const override;
    void definition(std::ostream &os) const;

    Type* type() const override;

    bool assignable() const override;

private:
    int index;
    Type *ty;
    bool temp;

    ASTNS::AST *_ast;
};

class Function : public Value
{
public:
    Function(FunctionType *ty, std::string name, ASTNS::Function *ast);

    void add(std::unique_ptr<Block> block);

    ASTNS::AST* ast() const override;
    std::string stringify() const override;
    void definition(std::ostream &os) const;
    void cfgDot(std::ostream &os) const;

    Type* type() const override;

    bool assignable() const override;

    std::vector<std::unique_ptr<Block>> blocks;

    Block* addBlock(std::string name);
    Register* addRegister(Type *ty, ASTNS::AST *ast, bool temp=true);

    FunctionType *ty;
    std::string name;

private:
    ASTNS::Function *_ast;

    size_t blocki;
    size_t regi;

    std::vector<std::unique_ptr<Register>> registers;
};

class ConstInt : public Value
{
public:
    ConstInt(BuiltinType *ty, ASTNS::AST *ast, int val);

    ASTNS::AST* ast() const override;
    std::string stringify() const override;

    Type* type() const override;

    bool assignable() const override;

private:
    int val;
    BuiltinType *ty;
    ASTNS::AST *_ast;
};

struct Local
{
    size_t scopenum;
    Value *v;
    std::string name;
};
