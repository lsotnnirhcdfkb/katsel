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

#include "llvm/IR/Type.h"
#include "llvm/IR/LLVMContext.h"

#include "codegen/codegen.h"

namespace IR
{
    // Base class {{{1
    class Type
    {
    public:
        inline Type(CodeGen::Context &context): context(context) {}
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
            doubleequal,
            bangequal
        };
        enum class UnaryOperator
        {
            bang,
            tilde,
            minus,
            doubleplus,
            doubleminus
        };

        virtual IR::ASTValue binOp(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, BinaryOperator op, IR::ASTValue l, IR::ASTValue r, Token optok, ASTNS::AST *ast) = 0;
        virtual IR::ASTValue unaryOp(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, UnaryOperator op, IR::ASTValue operand, Token optok, ASTNS::AST *ast) = 0;

        virtual IR::ASTValue implCast(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::ASTValue v) = 0;

        virtual IR::ASTValue castTo(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::ASTValue v, ASTNS::AST *ast) = 0;

        virtual llvm::Type* toLLVMType(llvm::LLVMContext &con) const = 0;

        CodeGen::Context &context;
    };
    // }}}
    // Float {{{1
    class FloatType : public Type
    {
    public:
        FloatType(CodeGen::Context &context, int size);

        std::string stringify() const override;

        IR::ASTValue binOp(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, BinaryOperator op, IR::ASTValue l, IR::ASTValue r, Token optok, ASTNS::AST *ast) override;
        IR::ASTValue unaryOp(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, UnaryOperator op, IR::ASTValue operand, Token optok, ASTNS::AST *ast) override;

        IR::ASTValue castTo(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::ASTValue v, ASTNS::AST *ast) override;

        IR::ASTValue implCast(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::ASTValue v) override;

        llvm::Type* toLLVMType(llvm::LLVMContext &context) const override;

        int size;
    };
    // Int {{{1
    class IntType : public Type
    {
    public:
        IntType(CodeGen::Context &context, int size, bool isSigned);

        std::string stringify() const override;

        IR::ASTValue binOp(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, BinaryOperator op, IR::ASTValue l, IR::ASTValue r, Token optok, ASTNS::AST *ast) override;
        IR::ASTValue unaryOp(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, UnaryOperator op, IR::ASTValue operand, Token optok, ASTNS::AST *ast) override;

        IR::ASTValue castTo(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::ASTValue v, ASTNS::AST *ast) override;

        IR::ASTValue implCast(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::ASTValue v) override;

        llvm::Type* toLLVMType(llvm::LLVMContext &context) const override;

        int size;
        bool isSigned;
    };
    // Char {{{1
    class CharType : public Type
    {
    public:
        CharType(CodeGen::Context &context);

        std::string stringify() const override;

        IR::ASTValue binOp(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, BinaryOperator op, IR::ASTValue l, IR::ASTValue r, Token optok, ASTNS::AST *ast) override;
        IR::ASTValue unaryOp(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, UnaryOperator op, IR::ASTValue operand, Token optok, ASTNS::AST *ast) override;

        IR::ASTValue castTo(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::ASTValue v, ASTNS::AST *ast) override;

        IR::ASTValue implCast(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::ASTValue v) override;

        llvm::Type* toLLVMType(llvm::LLVMContext &context) const override;
    };
    // Bool {{{1
    class BoolType : public Type
    {
    public:
        BoolType(CodeGen::Context &context);

        std::string stringify() const override;

        IR::ASTValue binOp(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, BinaryOperator op, IR::ASTValue l, IR::ASTValue r, Token optok, ASTNS::AST *ast) override;
        IR::ASTValue unaryOp(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, UnaryOperator op, IR::ASTValue operand, Token optok, ASTNS::AST *ast) override;

        IR::ASTValue castTo(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::ASTValue v, ASTNS::AST *ast) override;

        IR::ASTValue implCast(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::ASTValue v) override;

        llvm::Type* toLLVMType(llvm::LLVMContext &context) const override;
    };
    // Function {{{1
    class FunctionType : public Type
    {
    public:
        Type *ret;
        std::vector<Type*> paramtys;

        FunctionType(CodeGen::Context &context, Type *ret, std::vector<Type*> paramtys);
        std::string stringify() const override;

        IR::ASTValue binOp(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, BinaryOperator op, IR::ASTValue l, IR::ASTValue r, Token optok, ASTNS::AST *ast) override;
        IR::ASTValue unaryOp(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, UnaryOperator op, IR::ASTValue operand, Token optok, ASTNS::AST *ast) override;

        IR::ASTValue castTo(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::ASTValue v, ASTNS::AST *ast) override;

        IR::ASTValue implCast(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::ASTValue v) override;

        llvm::Type* toLLVMType(llvm::LLVMContext &context) const override;
    };
    // Void {{{1
    class VoidType : public Type
    {
    public:
        VoidType(CodeGen::Context &context);

        std::string stringify() const override;

        IR::ASTValue binOp(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, BinaryOperator op, IR::ASTValue l, IR::ASTValue r, Token optok, ASTNS::AST *ast) override;
        IR::ASTValue unaryOp(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, UnaryOperator op, IR::ASTValue operand, Token optok, ASTNS::AST *ast) override;

        IR::ASTValue castTo(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::ASTValue v, ASTNS::AST *ast) override;

        IR::ASTValue implCast(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::ASTValue v) override;

        llvm::Type* toLLVMType(llvm::LLVMContext &context) const override;
    };
    // Pointer {{{1
    class PointerType : public Type
    {
    public:
        PointerType(CodeGen::Context &context, Type *ty);

        std::string stringify() const override;

        IR::ASTValue binOp(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, BinaryOperator op, IR::ASTValue l, IR::ASTValue r, Token optok, ASTNS::AST *ast) override;
        IR::ASTValue unaryOp(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, UnaryOperator op, IR::ASTValue operand, Token optok, ASTNS::AST *ast) override;

        IR::ASTValue castTo(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::ASTValue v, ASTNS::AST *ast) override;

        IR::ASTValue implCast(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::ASTValue v) override;

        llvm::Type* toLLVMType(llvm::LLVMContext &context) const override;

        Type *ty;
    };
    // Generic literal types {{{1
    // Int {{{2
    class GenericIntType : public Type
    {
    public:
        GenericIntType(CodeGen::Context &context);

        std::string stringify() const override;

        IR::ASTValue binOp(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, BinaryOperator op, IR::ASTValue l, IR::ASTValue r, Token optok, ASTNS::AST *ast) override;
        IR::ASTValue unaryOp(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, UnaryOperator op, IR::ASTValue operand, Token optok, ASTNS::AST *ast) override;

        IR::ASTValue castTo(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::ASTValue v, ASTNS::AST *ast) override;

        IR::ASTValue implCast(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::ASTValue v) override;

        llvm::Type* toLLVMType(llvm::LLVMContext &context) const override;
    };
    // Float {{{2
    class GenericFloatType : public Type
    {
    public:
        GenericFloatType(CodeGen::Context &context);

        std::string stringify() const override;

        IR::ASTValue binOp(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, BinaryOperator op, IR::ASTValue l, IR::ASTValue r, Token optok, ASTNS::AST *ast) override;
        IR::ASTValue unaryOp(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, UnaryOperator op, IR::ASTValue operand, Token optok, ASTNS::AST *ast) override;

        IR::ASTValue castTo(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::ASTValue v, ASTNS::AST *ast) override;

        IR::ASTValue implCast(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::ASTValue v) override;

        llvm::Type* toLLVMType(llvm::LLVMContext &context) const override;
    };
    // }}}1
}

std::ostream& operator<<(std::ostream &os, IR::Type const *t);
