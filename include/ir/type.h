#pragma once

#include <vector>
#include <map>
namespace IR
{
    class Value;
}

#include "lex/token.h"
#include "lex/tokentype.h"
#include "ast/ast.h"

namespace CodeGenNS
{
    class Context;
}

namespace IR
{
    class Type
    {
    public:
        virtual ~Type() {};
        virtual std::string stringify() = 0;
        virtual bool hasOperator(TokenType t) = 0;

        virtual IR::Value* binOp(CodeGenNS::Context &cgc, IR::Value *l, IR::Value *r, Token op, ASTNS::AST *ast) = 0;
        virtual IR::Value* unaryOp(CodeGenNS::Context &cgc, IR::Value *operand, Token op, ASTNS::AST *ast) = 0;
        virtual IR::Value* isTrue(CodeGenNS::Context &cgc, IR::Value *v) = 0;

        virtual IR::Value* castTo(CodeGenNS::Context &cgc, IR::Value *v) = 0;

        ASTNS::TypeB *ast;
    };

    class BuiltinType : public Type
    {
    public:
        enum class Builtins
        {
            UINT8,
            UINT16,
            UINT32,
            UINT64,
            SINT8,
            SINT16,
            SINT32,
            SINT64,

            FLOAT,
            CHAR,
            BOOL,
            DOUBLE
        };
        Builtins type;

        BuiltinType(Builtins b);
        std::string stringify() override;

        bool hasOperator(TokenType t) override;

        IR::Value* binOp(CodeGenNS::Context &cgc, IR::Value *l, IR::Value *r, Token op, ASTNS::AST *ast) override;
        IR::Value* unaryOp(CodeGenNS::Context &cgc, IR::Value *operand, Token op, ASTNS::AST *ast) override;
        IR::Value* isTrue(CodeGenNS::Context &cgc, IR::Value *v) override;

        IR::Value* castTo(CodeGenNS::Context &cgc, IR::Value *v) override;
    };


    class FunctionType : public Type
    {
    public:
        Type *ret;
        std::vector<Type*> paramtys;

        FunctionType(Type *ret, std::vector<Type*> paramtys);
        std::string stringify() override;
        bool hasOperator(TokenType t) override;
        IR::Value* binOp(CodeGenNS::Context &cgc, IR::Value *l, IR::Value *r, Token op, ASTNS::AST *ast) override;
        IR::Value* unaryOp(CodeGenNS::Context &cgc, IR::Value *operand, Token op, ASTNS::AST *ast) override;
        IR::Value* isTrue(CodeGenNS::Context &cgc, IR::Value *v) override;

        IR::Value* castTo(CodeGenNS::Context &cgc, IR::Value *v) override;
    };

    class VoidType : public Type
    {
    public:
        std::string stringify() override;
        bool hasOperator(TokenType t) override;
        IR::Value* binOp(CodeGenNS::Context &cgc, IR::Value *l, IR::Value *r, Token op, ASTNS::AST *ast) override;
        IR::Value* unaryOp(CodeGenNS::Context &cgc, IR::Value *operand, Token op, ASTNS::AST *ast) override;
        IR::Value* isTrue(CodeGenNS::Context &cgc, IR::Value *v) override;

        IR::Value* castTo(CodeGenNS::Context &cgc, IR::Value *v) override;
    };
}
