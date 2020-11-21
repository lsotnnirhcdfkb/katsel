#pragma once

#include "ir/type.h"
#include "ir/block.h"
#include <string>
#include <cstddef>

class Value
{
public:
    virtual ~Value() {};
    virtual ASTNS::AST* ast() const;
};

class Register : public Value
{
public:
    Register(int index, Type *type);
    std::string stringify();

    ASTNS::AST* ast() const override;

private:
    int index;

    Type *type;
};

class Function : public Value
{
public:
    Function(FunctionType *ty, std::string name);

    void add(std::unique_ptr<Block> block);

    ASTNS::AST* ast() const override;

private:
    std::vector<std::unique_ptr<Block>> blocks;
    FunctionType *ty;
    std::string name;
};
