#pragma once

#include <vector>
#include <map>
namespace IR
{
    struct ASTValue;
    class Block;
    class Function;
}

#include "lex/token.h"
#include "lex/tokentype.h"
#include "ast/ast.h"

#include "llvm/IR/Type.h"
#include "llvm/IR/LLVMContext.h"

#include "codegen/codegen.h"

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

        virtual IR::ASTValue binOp(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, BinaryOperator op, IR::ASTValue l, IR::ASTValue r, Token optok, ASTNS::AST *ast) = 0;
        virtual IR::ASTValue unaryOp(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, UnaryOperator op, IR::ASTValue operand, Token optok, ASTNS::AST *ast) = 0;

        virtual IR::ASTValue isTrue(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::ASTValue v) = 0;

        virtual IR::ASTValue castTo(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::ASTValue v, ASTNS::AST *ast) = 0;

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

        IR::ASTValue binOp(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, BinaryOperator op, IR::ASTValue l, IR::ASTValue r, Token optok, ASTNS::AST *ast) override;
        IR::ASTValue unaryOp(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, UnaryOperator op, IR::ASTValue operand, Token optok, ASTNS::AST *ast) override;

        IR::ASTValue isTrue(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::ASTValue v) override;

        IR::ASTValue castTo(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::ASTValue v, ASTNS::AST *ast) override;

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

        IR::ASTValue binOp(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, BinaryOperator op, IR::ASTValue l, IR::ASTValue r, Token optok, ASTNS::AST *ast) override;
        IR::ASTValue unaryOp(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, UnaryOperator op, IR::ASTValue operand, Token optok, ASTNS::AST *ast) override;

        IR::ASTValue isTrue(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::ASTValue v) override;

        IR::ASTValue castTo(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::ASTValue v, ASTNS::AST *ast) override;

        llvm::Type* toLLVMType(llvm::LLVMContext &con) const override;
    };

    class VoidType : public Type
    {
    public:
        std::string stringify() const override;

        IR::ASTValue binOp(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, BinaryOperator op, IR::ASTValue l, IR::ASTValue r, Token optok, ASTNS::AST *ast) override;
        IR::ASTValue unaryOp(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, UnaryOperator op, IR::ASTValue operand, Token optok, ASTNS::AST *ast) override;

        IR::ASTValue isTrue(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::ASTValue v) override;

        IR::ASTValue castTo(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::ASTValue v, ASTNS::AST *ast) override;

        llvm::Type* toLLVMType(llvm::LLVMContext &con) const override;
    };
}

std::ostream& operator<<(std::ostream &os, IR::Type const *t);
