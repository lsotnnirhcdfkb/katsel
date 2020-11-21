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

    virtual Type* type() const = 0;
};

class Register : public Value
{
public:
    Register(int index, Type *type, ASTNS::AST *ast);

    ASTNS::AST* ast() const override;
    std::string stringify() const override;

    Type* type() const override;

private:
    int index;
    Type *ty;

    ASTNS::AST *_ast;
};

class Function : public Value
{
public:
    Function(FunctionType *ty, std::string name, ASTNS::Function *ast);

    void add(std::unique_ptr<Block> block);

    ASTNS::AST* ast() const override;
    std::string stringify() const override;

    Type* type() const override;

    std::vector<std::unique_ptr<Block>> blocks;

    Block* addBlock(std::string name);

private:
    FunctionType *ty;
    std::string name;

    ASTNS::Function *_ast;

    size_t blocki;

    std::vector<std::unique_ptr<Register>> registers;
};

struct Local
{
    size_t scopenum;
    Value *v;
    std::string name;
};
