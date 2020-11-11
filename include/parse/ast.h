#pragma once

#include <memory>
#include <vector>
#include "lex/token.h"
#include "visit/visitor.h"

namespace ASTNS
{
// ASTHEADER START

// The following code was autogenerated - see the utils/ directory
    class AST;
    class add;
    class expr;
    class mult;
    class primary;
    class stmt;
    class unary;
    class AST
    {
    public:
        virtual ~AST() {}
        virtual void accept(ASTVisitor *v) = 0;
    };
    class add : public AST
    {
    public:
        add(std::unique_ptr<AST> lhs, Token op, std::unique_ptr<AST> rhs);
        std::unique_ptr<AST> lhs;
        Token op;
        std::unique_ptr<AST> rhs;
        virtual void accept(ASTVisitor *v);
    };
    class expr : public AST
    {
    public:
        expr(std::unique_ptr<AST> expr);
        std::unique_ptr<AST> expr;
        virtual void accept(ASTVisitor *v);
    };
    class mult : public AST
    {
    public:
        mult(std::unique_ptr<AST> lhs, Token op, std::unique_ptr<AST> rhs);
        std::unique_ptr<AST> lhs;
        Token op;
        std::unique_ptr<AST> rhs;
        virtual void accept(ASTVisitor *v);
    };
    class primary : public AST
    {
    public:
        primary(Token value);
        primary(Token oparn, std::unique_ptr<AST> expr, Token cparn);
        Token value;
        Token oparn;
        std::unique_ptr<AST> expr;
        Token cparn;
        virtual void accept(ASTVisitor *v);
    };
    class stmt : public AST
    {
    public:
        stmt(std::unique_ptr<AST> expr);
        std::unique_ptr<AST> expr;
        virtual void accept(ASTVisitor *v);
    };
    class unary : public AST
    {
    public:
        unary(Token op, std::unique_ptr<AST> operand);
        Token op;
        std::unique_ptr<AST> operand;
        virtual void accept(ASTVisitor *v);
    };
// This code was autogenerated - see the utils/ directory

// ASTHEADER END
}
