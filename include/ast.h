#pragma once

#include <vector>
#include <string>
#include <iostream>
#include <memory>
#include "token.h"
#include "visitor.h"

class AST
{
public:
    virtual ~AST() {}

    virtual void accept(Visitor *v) = 0;
};

class BinaryAST : public AST
{
public:
    BinaryAST(Token op, std::unique_ptr<AST> last, std::unique_ptr<AST> rast);
    void accept(Visitor *v) override;

    Token op;
    std::unique_ptr<AST> last, rast;
};

class TernaryOpAST : public AST
{
    // is reserved only for ?: ternary operator so only needs to store operands
public:
    TernaryOpAST(std::unique_ptr<AST> conditional, std::unique_ptr<AST> trueast, std::unique_ptr<AST> falseast);
    void accept(Visitor *v) override;

    std::unique_ptr<AST> conditional, trueast, falseast;
};

class UnaryAST : public AST
{
public:
    UnaryAST(Token op, std::unique_ptr<AST> ast);
    void accept(Visitor *v) override;

    Token op;
    std::unique_ptr<AST> ast;
};

class PrimaryAST : public AST
{
public:
    PrimaryAST(Token value);
    void accept(Visitor *v) override;

    Token value;
};

class ExprStmtAST : public AST
{
public:
    ExprStmtAST(std::unique_ptr<AST> ast);
    void accept(Visitor *v) override;

    std::unique_ptr<AST> ast;
};

class ProgramAST : public AST
{
public:
    ProgramAST(std::vector<std::unique_ptr<AST>> &asts);
    void accept(Visitor *v) override;

    std::vector<std::unique_ptr<AST>> asts;
};
