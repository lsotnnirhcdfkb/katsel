#pragma once

#include <vector>
#include <map>

namespace IR {
    struct ASTValue;
    class Block;
    class Function;
}

#include "ir/module.h"

#include "lex/token.h"
#include "lex/tokentype.h"

#include "llvm/IR/Type.h"
#include "llvm/IR/LLVMContext.h"

#include "codegen/codegen.h"

#include "message/reportAbort.h"

#define DERIVE_TYPE_DECL() \
    public: \
        IR::ASTValue binOp(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, BinaryOperator op, IR::ASTValue l, IR::ASTValue r, Token optok, ASTNS::AST *ast) override; \
        IR::ASTValue unaryOp(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, UnaryOperator op, IR::ASTValue operand, Token optok, ASTNS::AST *ast) override; \
        IR::ASTValue implCast(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::ASTValue v) override; \
        IR::ASTValue castFrom(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::ASTValue v, ASTNS::AST *ast) override; \
        llvm::Type* toLLVMType(llvm::LLVMContext &con) const override; \
        void type_accept(IR::TypeVisitor *v) override;

// i learned about this from http://journal.stuffwithstuff.com/2012/01/24/higher-order-macros-in-c/
#define IR_TYPES(mac) \
    mac(FloatType) \
    mac(IntType) \
    mac(CharType) \
    mac(BoolType) \
    mac(FunctionType) \
    mac(VoidType) \
    mac(PointerType) \
    mac(GenericIntType) \
    mac(GenericFloatType)

namespace IR {
    class TypeVisitor;

    // Base class {{{1
    class Type : public DeclSymbol {
    public:
        inline Type(CodeGen::Context &context): context(context) {}
        virtual ~Type() {}

        enum class BinaryOperator {
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
        enum class UnaryOperator {
            bang,
            tilde,
            minus,
            doubleplus,
            doubleminus
        };

        virtual IR::ASTValue binOp(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, BinaryOperator op, IR::ASTValue l, IR::ASTValue r, Token optok, ASTNS::AST *ast) = 0;
        virtual IR::ASTValue unaryOp(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, UnaryOperator op, IR::ASTValue operand, Token optok, ASTNS::AST *ast) = 0;

        virtual IR::ASTValue implCast(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::ASTValue v) = 0;

        virtual IR::ASTValue castFrom(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::ASTValue v, ASTNS::AST *ast) = 0;

        virtual llvm::Type* toLLVMType(llvm::LLVMContext &con) const = 0;

        virtual void type_accept(TypeVisitor *v) = 0;

        void declsym_accept(DeclSymbolVisitor *v) override;

        CodeGen::Context &context;
    };
    // }}}
    // Float {{{1
    class FloatType : public Type {
    public:
        FloatType(CodeGen::Context &context, ASTNS::AST *declAST, int size);

        std::string name() const override;
        ASTNS::AST *declAST() const override;

        int size;

        DERIVE_DECLSYMBOL_ITEMS_DECL()

        DERIVE_TYPE_DECL()

    private:
        ASTNS::AST *_declAST;
    };
    // Int {{{1
    class IntType : public Type {
    public:
        IntType(CodeGen::Context &context, ASTNS::AST *declAST, int size, bool isSigned);

        std::string name() const override;
        ASTNS::AST *declAST() const override;

        DERIVE_TYPE_DECL()

        int size;
        bool isSigned;

        DERIVE_DECLSYMBOL_ITEMS_DECL()

    private:
        ASTNS::AST *_declAST;
    };
    // Char {{{1
    class CharType : public Type {
    public:
        CharType(CodeGen::Context &context, ASTNS::AST *declAST);

        std::string name() const override;

        DERIVE_TYPE_DECL()

        ASTNS::AST *declAST() const override;

        DERIVE_DECLSYMBOL_ITEMS_DECL()

    private:
        ASTNS::AST *_declAST;
    };
    // Bool {{{1
    class BoolType : public Type {
    public:
        BoolType(CodeGen::Context &context, ASTNS::AST *declAST);

        std::string name() const override;

        DERIVE_TYPE_DECL()

        ASTNS::AST *declAST() const override;

        DERIVE_DECLSYMBOL_ITEMS_DECL()

    private:
        ASTNS::AST *_declAST;
    };
    // Function {{{1
    class FunctionType : public Type {
    public:
        Type *ret;
        std::vector<Type*> paramtys;

        FunctionType(CodeGen::Context &context, ASTNS::AST *declAST, Type *ret, std::vector<Type*> paramtys);
        std::string name() const override;

        DERIVE_TYPE_DECL()

        ASTNS::AST *declAST() const override;

        DERIVE_DECLSYMBOL_ITEMS_DECL()

    private:
        ASTNS::AST *_declAST;
    };
    // Void {{{1
    class VoidType : public Type {
    public:
        VoidType(CodeGen::Context &context, ASTNS::AST *declAST);

        std::string name() const override;

        DERIVE_TYPE_DECL()

        ASTNS::AST *declAST() const override;

        DERIVE_DECLSYMBOL_ITEMS_DECL()

    private:
        ASTNS::AST *_declAST;
    };
    // Pointer {{{1
    class PointerType : public Type {
    public:
        PointerType(CodeGen::Context &context, ASTNS::AST *declAST, bool mut, Type *ty);

        std::string name() const override;

        DERIVE_TYPE_DECL()

        Type *ty;
        bool mut;

        ASTNS::AST *declAST() const override;

        DERIVE_DECLSYMBOL_ITEMS_DECL()

    private:
        ASTNS::AST *_declAST;
    };
    // Generic literal types {{{1
    // Int {{{2
    class GenericIntType : public Type {
    public:
        GenericIntType(CodeGen::Context &context, ASTNS::AST *declAST);

        std::string name() const override;

        DERIVE_TYPE_DECL()

        ASTNS::AST *declAST() const override;

        DERIVE_DECLSYMBOL_ITEMS_DECL()

    private:
        ASTNS::AST *_declAST;
    };
    // Float {{{2
    class GenericFloatType : public Type {
    public:
        GenericFloatType(CodeGen::Context &context, ASTNS::AST *declAST);

        std::string name() const override;

        DERIVE_TYPE_DECL()

        ASTNS::AST *declAST() const override;

        DERIVE_DECLSYMBOL_ITEMS_DECL()

    private:
        ASTNS::AST *_declAST;
    };
    // }}}1

    class TypeVisitor {
    public:
        virtual ~TypeVisitor() {}
#define VISITTY(cl) virtual void type_visit##cl(cl *ty) = 0;
        IR_TYPES(VISITTY)
#undef VISITTY
    };
}

std::ostream& operator<<(std::ostream &os, IR::Type const *t);
