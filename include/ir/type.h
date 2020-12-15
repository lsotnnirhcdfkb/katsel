#pragma once

#include <vector>
#include <map>
namespace IR
{
    struct ASTValue;
    class Block;
}

#include "lex/token.h"
#include "lex/tokentype.h"
#include "ast/ast.h"

#include "llvm/IR/Type.h"
#include "llvm/IR/LLVMContext.h"

namespace CodeGenNS
{
    class Context;
}

namespace IR
{
    class Type
    {
    public:
        virtual ~Type() {}
        virtual std::string stringify() const = 0;

        enum class BinaryOperator
        {
            plus,
            minus,
            star,
            slash,
            percent,
            greater,
            less,
            greaterequal,
            lessequal,
            amper,
            pipe,
            caret,
            doublegreater,
            doubleless,
            doubleamper,
            doublepipe,
            doubleequal,
            bangequal
        };
        enum class UnaryOperator
        {
            bang,
            tilde,
            minus,
            postfixdoubleplus,
            postfixdoubleminus,
            prefixdoubleplus,
            prefixdoubleminus
        };

        virtual IR::ASTValue binOp(IR::Block *&curBlock, BinaryOperator op, IR::ASTValue l, IR::ASTValue r, Token optok, ASTNS::AST *ast) = 0;
        virtual IR::ASTValue unaryOp(IR::Block *&curBlock, UnaryOperator op, IR::ASTValue operand, Token optok, ASTNS::AST *ast) = 0;

        virtual IR::ASTValue isTrue(IR::Block *&curBlock, IR::ASTValue v) = 0;

        virtual IR::ASTValue castTo(IR::Block *&curBlock, IR::ASTValue v, ASTNS::AST *ast) = 0;

        ASTNS::TypeB *ast;

        virtual llvm::Type* toLLVMType(llvm::LLVMContext &con) const = 0;
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
        std::string stringify() const override;

        IR::ASTValue binOp(IR::Block *&curBlock, BinaryOperator op, IR::ASTValue l, IR::ASTValue r, Token optok, ASTNS::AST *ast) override;
        IR::ASTValue unaryOp(IR::Block *&curBlock, UnaryOperator op, IR::ASTValue operand, Token optok, ASTNS::AST *ast) override;

        IR::ASTValue isTrue(IR::Block *&curBlock, IR::ASTValue v) override;

        IR::ASTValue castTo(IR::Block *&curBlock, IR::ASTValue v, ASTNS::AST *ast) override;

        llvm::Type* toLLVMType(llvm::LLVMContext &con) const override;

        bool isFloating();
        bool isSigned();
    };


    class FunctionType : public Type
    {
    public:
        Type *ret;
        std::vector<Type*> paramtys;

        FunctionType(Type *ret, std::vector<Type*> paramtys);
        std::string stringify() const override;

        IR::ASTValue binOp(IR::Block *&curBlock, BinaryOperator op, IR::ASTValue l, IR::ASTValue r, Token optok, ASTNS::AST *ast) override;
        IR::ASTValue unaryOp(IR::Block *&curBlock, UnaryOperator op, IR::ASTValue operand, Token optok, ASTNS::AST *ast) override;

        IR::ASTValue isTrue(IR::Block *&curBlock, IR::ASTValue v) override;

        IR::ASTValue castTo(IR::Block *&curBlock, IR::ASTValue v, ASTNS::AST *ast) override;

        llvm::Type* toLLVMType(llvm::LLVMContext &con) const override;
    };

    class VoidType : public Type
    {
    public:
        std::string stringify() const override;

        IR::ASTValue binOp(IR::Block *&curBlock, BinaryOperator op, IR::ASTValue l, IR::ASTValue r, Token optok, ASTNS::AST *ast) override;
        IR::ASTValue unaryOp(IR::Block *&curBlock, UnaryOperator op, IR::ASTValue operand, Token optok, ASTNS::AST *ast) override;

        IR::ASTValue isTrue(IR::Block *&curBlock, IR::ASTValue v) override;

        IR::ASTValue castTo(IR::Block *&curBlock, IR::ASTValue v, ASTNS::AST *ast) override;

        llvm::Type* toLLVMType(llvm::LLVMContext &con) const override;
    };
}

std::ostream& operator<<(std::ostream &os, IR::Type const *t);
