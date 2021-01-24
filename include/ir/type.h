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
        Maybe<IR::ASTValue> binOp(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &curBlock, BinaryOperator op, IR::ASTValue l, IR::ASTValue r, Token optok, NNPtr<ASTNS::AST> ast) override; \
        Maybe<IR::ASTValue> unaryOp(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &curBlock, UnaryOperator op, IR::ASTValue operand, Token optok, NNPtr<ASTNS::AST> ast) override; \
        IR::ASTValue implCast(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &curBlock, IR::ASTValue v) override; \
        Maybe<IR::ASTValue> castFrom(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &curBlock, IR::ASTValue v, NNPtr<ASTNS::AST> ast) override; \
        NNPtr<llvm::Type> toLLVMType(llvm::LLVMContext &con) const override; \
        void type_accept(IR::TypeVisitor &v) override; \
        virtual Maybe<Method const> getMethod(std::string const &name) const override; \
        virtual void addMethod(std::string const &name, Method const &m) override; \
        virtual bool hasField(std::string const &name) const override; \
        virtual int getFieldIndex(std::string const &name) const override;

#define DERIVE_TYPE_METHOD_TABLE_IMPL(cl) \
    Maybe<IR::Type::Method const> cl::getMethod(std::string const &name) const { \
        auto m = methods.find(name); \
        if (m == methods.end()) \
            return Maybe<IR::Type::Method const>(); \
        return Maybe<IR::Type::Method const>(m->second); \
    } \
    void cl::addMethod(std::string const &name, IR::Type::Method const &m) { \
        if (methods.find(name) != methods.end()) \
            reportAbortNoh(format("add duplicate method in type " #cl " under name {}", name)); \
        methods.emplace(name, m); \
    }

#define DERIVE_TYPE_NO_FIELDS(cl) \
    bool cl::hasField(std::string const &name) const { \
        return false; \
    } \
    int cl::getFieldIndex(std::string const &name) const { \
        reportAbortNoh(#cl "::hasField() is constant false, but getFieldIndex() called"); \
    }


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

        virtual Maybe<IR::ASTValue> binOp(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &curBlock, BinaryOperator op, IR::ASTValue l, IR::ASTValue r, Token optok, NNPtr<ASTNS::AST> ast) = 0;
        virtual Maybe<IR::ASTValue> unaryOp(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &curBlock, UnaryOperator op, IR::ASTValue operand, Token optok, NNPtr<ASTNS::AST> ast) = 0;

        virtual IR::ASTValue implCast(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &curBlock, IR::ASTValue v) = 0;

        virtual Maybe<IR::ASTValue> castFrom(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &curBlock, IR::ASTValue v, NNPtr<ASTNS::AST> ast) = 0;

        virtual NNPtr<llvm::Type> toLLVMType(llvm::LLVMContext &con) const = 0;

        virtual void type_accept(TypeVisitor &v) = 0;

        void declsym_accept(DeclSymbolVisitor &v) override;

        struct Method {
            NNPtr<IR::Function> fun;
            bool thisPtr, thisMut;
        };
        virtual Maybe<Method const> getMethod(std::string const &name) const = 0;
        virtual void addMethod(std::string const &name, Method const &m) = 0;

        virtual bool hasField(std::string const &name) const = 0;
        virtual int getFieldIndex(std::string const &name) const = 0;

        CodeGen::Context &context;
    };
    // }}}
    // Float {{{1
    class FloatType : public Type {
    public:
        FloatType(CodeGen::Context &context, NNPtr<ASTNS::AST> declAST, int size);

        std::string name() const override;
        NNPtr<ASTNS::AST> declAST() const override;

        int size;

        DERIVE_DECLSYMBOL_ITEMS_DECL()

        DERIVE_TYPE_DECL()

    private:
        NNPtr<ASTNS::AST> _declAST;

        std::map<std::string, IR::Type::Method> methods;
    };
    // Int {{{1
    class IntType : public Type {
    public:
        IntType(CodeGen::Context &context, NNPtr<ASTNS::AST> declAST, int size, bool isSigned);

        std::string name() const override;
        NNPtr<ASTNS::AST> declAST() const override;

        DERIVE_TYPE_DECL()

        int size;
        bool isSigned;

        DERIVE_DECLSYMBOL_ITEMS_DECL()

    private:
        NNPtr<ASTNS::AST> _declAST;

        std::map<std::string, IR::Type::Method> methods;
    };
    // Char {{{1
    class CharType : public Type {
    public:
        CharType(CodeGen::Context &context, NNPtr<ASTNS::AST> declAST);

        std::string name() const override;

        DERIVE_TYPE_DECL()

        NNPtr<ASTNS::AST> declAST() const override;

        DERIVE_DECLSYMBOL_ITEMS_DECL()

    private:
        NNPtr<ASTNS::AST> _declAST;

        std::map<std::string, IR::Type::Method> methods;
    };
    // Bool {{{1
    class BoolType : public Type {
    public:
        BoolType(CodeGen::Context &context, NNPtr<ASTNS::AST> declAST);

        std::string name() const override;

        DERIVE_TYPE_DECL()

        NNPtr<ASTNS::AST> declAST() const override;

        DERIVE_DECLSYMBOL_ITEMS_DECL()

    private:
        NNPtr<ASTNS::AST> _declAST;

        std::map<std::string, IR::Type::Method> methods;
    };
    // Function {{{1
    class FunctionType : public Type {
    public:
        NNPtr<Type> ret;
        std::vector<NNPtr<Type>> paramtys;

        FunctionType(CodeGen::Context &context, NNPtr<ASTNS::AST> declAST, NNPtr<Type> ret, std::vector<NNPtr<Type>> paramtys);
        std::string name() const override;

        DERIVE_TYPE_DECL()

        NNPtr<ASTNS::AST> declAST() const override;

        DERIVE_DECLSYMBOL_ITEMS_DECL()

    private:
        NNPtr<ASTNS::AST> _declAST;

        std::map<std::string, IR::Type::Method> methods;
    };
    // Void {{{1
    class VoidType : public Type {
    public:
        VoidType(CodeGen::Context &context, NNPtr<ASTNS::AST> declAST);

        std::string name() const override;

        DERIVE_TYPE_DECL()

        NNPtr<ASTNS::AST> declAST() const override;

        DERIVE_DECLSYMBOL_ITEMS_DECL()

    private:
        NNPtr<ASTNS::AST> _declAST;

        std::map<std::string, IR::Type::Method> methods;
    };
    // Pointer {{{1
    class PointerType : public Type {
    public:
        PointerType(CodeGen::Context &context, NNPtr<ASTNS::AST> declAST, bool mut, NNPtr<Type> ty);

        std::string name() const override;

        DERIVE_TYPE_DECL()

        NNPtr<Type> ty;
        bool mut;

        NNPtr<ASTNS::AST> declAST() const override;

        DERIVE_DECLSYMBOL_ITEMS_DECL()

    private:
        NNPtr<ASTNS::AST> _declAST;

        std::map<std::string, IR::Type::Method> methods;
    };
    // Generic literal types {{{1
    // Int {{{2
    class GenericIntType : public Type {
    public:
        GenericIntType(CodeGen::Context &context, NNPtr<ASTNS::AST> declAST);

        std::string name() const override;

        DERIVE_TYPE_DECL()

        NNPtr<ASTNS::AST> declAST() const override;

        DERIVE_DECLSYMBOL_ITEMS_DECL()

    private:
        NNPtr<ASTNS::AST> _declAST;

        std::map<std::string, IR::Type::Method> methods;
    };
    // Float {{{2
    class GenericFloatType : public Type {
    public:
        GenericFloatType(CodeGen::Context &context, NNPtr<ASTNS::AST> declAST);

        std::string name() const override;

        DERIVE_TYPE_DECL()

        NNPtr<ASTNS::AST> declAST() const override;

        DERIVE_DECLSYMBOL_ITEMS_DECL()

    private:
        NNPtr<ASTNS::AST> _declAST;

        std::map<std::string, IR::Type::Method> methods;
    };
    // }}}1

    class TypeVisitor {
    public:
        virtual ~TypeVisitor() {}
#define VISITTY(cl) virtual void type_visit##cl(cl &ty) = 0;
        IR_TYPES(VISITTY)
#undef VISITTY
    };
}

std::ostream& operator<<(std::ostream &os, NNPtr<IR::Type> const &t);
std::ostream& operator<<(std::ostream &os, NNPtr<IR::Type const> const &t);
