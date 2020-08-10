#pragma once

#include <vector>
#include <string>
#include <iostream>
#include <memory>
#include "token.h"

class AST
{
public:
    virtual ~AST() {}
};

class BinaryAST : public AST
{
public:
    BinaryAST(Token op, std::unique_ptr<AST> &last, std::unique_ptr<AST> &rast);

private:
    Token op;
    std::unique_ptr<AST> last, rast;
};

class TernaryOpAST : public AST
{
    // is reserved only for ?: ternary operator so only needs to store operands
public:
    TernaryOpAST(std::unique_ptr<AST> &conditional, std::unique_ptr<AST> &trueast, std::unique_ptr<AST> &falseast);

private:
    std::unique_ptr<AST> conditional, trueast, falseast;
};

class UnaryAST : public AST
{
public:
    UnaryAST(Token op, std::unique_ptr<AST> &ast);

private:
    Token op;
    std::unique_ptr<AST> ast;
};

class PrimaryAST : public AST
{
public:
    PrimaryAST(Token value);

private:
    Token value;
};

