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

#include "message/report_abort.h"

#define DERIVE_TYPE_DECL() \
    public: \
        Maybe<IR::ASTValue> bin_op(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, BinaryOperator op, IR::ASTValue l, IR::ASTValue r, Token optok, NNPtr<ASTNS::AST> ast) override; \
        Maybe<IR::ASTValue> unary_op(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, UnaryOperator op, IR::ASTValue operand, Token optok, NNPtr<ASTNS::AST> ast) override; \
        IR::ASTValue impl_cast(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, IR::ASTValue v) override; \
        Maybe<IR::ASTValue> cast_from(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, IR::ASTValue v, NNPtr<ASTNS::AST> ast) override; \
        NNPtr<llvm::Type> to_llvmtype(llvm::LLVMContext &con) const override; \
        void type_accept(IR::TypeVisitor &v) override; \
        virtual Maybe<Method const> get_method(std::string const &name) const override; \
        virtual void add_method(std::string const &name, Method const &m) override; \
        virtual bool has_field(std::string const &name) const override; \
        virtual int get_field_index(std::string const &name) const override;

#define DERIVE_TYPE_METHOD_TABLE_IMPL(cl) \
    Maybe<IR::Type::Method const> cl::get_method(std::string const &name) const { \
        auto m = methods.find(name); \
        if (m == methods.end()) \
            return Maybe<IR::Type::Method const>(); \
        return Maybe<IR::Type::Method const>(m->second); \
    } \
    void cl::add_method(std::string const &name, IR::Type::Method const &m) { \
        if (methods.find(name) != methods.end()) \
            report_abort_noh(format("add duplicate method in type " #cl " under name {}", name)); \
        methods.emplace(name, m); \
    }

#define DERIVE_TYPE_NO_FIELDS(cl) \
    bool cl::has_field(std::string const &name) const { \
        return false; \
    } \
    int cl::get_field_index(std::string const &name) const { \
        report_abort_noh(#cl "::has_field() is constant false, but get_field_index() called"); \
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

        virtual Maybe<IR::ASTValue> bin_op(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, BinaryOperator op, IR::ASTValue l, IR::ASTValue r, Token optok, NNPtr<ASTNS::AST> ast) = 0;
        virtual Maybe<IR::ASTValue> unary_op(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, UnaryOperator op, IR::ASTValue operand, Token optok, NNPtr<ASTNS::AST> ast) = 0;

        virtual IR::ASTValue impl_cast(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, IR::ASTValue v) = 0;

        virtual Maybe<IR::ASTValue> cast_from(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, IR::ASTValue v, NNPtr<ASTNS::AST> ast) = 0;

        virtual NNPtr<llvm::Type> to_llvmtype(llvm::LLVMContext &con) const = 0;

        virtual void type_accept(TypeVisitor &v) = 0;

        void declsym_accept(DeclSymbolVisitor &v) override;

        struct Method {
            NNPtr<IR::Function> fun;
            bool this_ptr, this_mut;
        };
        virtual Maybe<Method const> get_method(std::string const &name) const = 0;
        virtual void add_method(std::string const &name, Method const &m) = 0;

        virtual bool has_field(std::string const &name) const = 0;
        virtual int get_field_index(std::string const &name) const = 0;

        CodeGen::Context &context;
    };
    // }}}
    // Float {{{1
    class FloatType : public Type {
    public:
        FloatType(CodeGen::Context &context, NNPtr<ASTNS::AST> decl_ast, int size);

        std::string name() const override;
        NNPtr<ASTNS::AST> decl_ast() const override;

        int size;

        DERIVE_DECLSYMBOL_ITEMS_DECL()

        DERIVE_TYPE_DECL()

    private:
        NNPtr<ASTNS::AST> _decl_ast;

        std::map<std::string, IR::Type::Method> methods;
    };
    // Int {{{1
    class IntType : public Type {
    public:
        IntType(CodeGen::Context &context, NNPtr<ASTNS::AST> decl_ast, int size, bool is_signed);

        std::string name() const override;
        NNPtr<ASTNS::AST> decl_ast() const override;

        DERIVE_TYPE_DECL()

        int size;
        bool is_signed;

        DERIVE_DECLSYMBOL_ITEMS_DECL()

    private:
        NNPtr<ASTNS::AST> _decl_ast;

        std::map<std::string, IR::Type::Method> methods;
    };
    // Char {{{1
    class CharType : public Type {
    public:
        CharType(CodeGen::Context &context, NNPtr<ASTNS::AST> decl_ast);

        std::string name() const override;

        DERIVE_TYPE_DECL()

        NNPtr<ASTNS::AST> decl_ast() const override;

        DERIVE_DECLSYMBOL_ITEMS_DECL()

    private:
        NNPtr<ASTNS::AST> _decl_ast;

        std::map<std::string, IR::Type::Method> methods;
    };
    // Bool {{{1
    class BoolType : public Type {
    public:
        BoolType(CodeGen::Context &context, NNPtr<ASTNS::AST> decl_ast);

        std::string name() const override;

        DERIVE_TYPE_DECL()

        NNPtr<ASTNS::AST> decl_ast() const override;

        DERIVE_DECLSYMBOL_ITEMS_DECL()

    private:
        NNPtr<ASTNS::AST> _decl_ast;

        std::map<std::string, IR::Type::Method> methods;
    };
    // Function {{{1
    class FunctionType : public Type {
    public:
        NNPtr<Type> ret;
        std::vector<NNPtr<Type>> paramtys;

        FunctionType(CodeGen::Context &context, NNPtr<ASTNS::AST> decl_ast, NNPtr<Type> ret, std::vector<NNPtr<Type>> paramtys);
        std::string name() const override;

        DERIVE_TYPE_DECL()

        NNPtr<ASTNS::AST> decl_ast() const override;

        DERIVE_DECLSYMBOL_ITEMS_DECL()

    private:
        NNPtr<ASTNS::AST> _decl_ast;

        std::map<std::string, IR::Type::Method> methods;
    };
    // Void {{{1
    class VoidType : public Type {
    public:
        VoidType(CodeGen::Context &context, NNPtr<ASTNS::AST> decl_ast);

        std::string name() const override;

        DERIVE_TYPE_DECL()

        NNPtr<ASTNS::AST> decl_ast() const override;

        DERIVE_DECLSYMBOL_ITEMS_DECL()

    private:
        NNPtr<ASTNS::AST> _decl_ast;

        std::map<std::string, IR::Type::Method> methods;
    };
    // Pointer {{{1
    class PointerType : public Type {
    public:
        PointerType(CodeGen::Context &context, NNPtr<ASTNS::AST> decl_ast, bool mut, NNPtr<Type> ty);

        std::string name() const override;

        DERIVE_TYPE_DECL()

        NNPtr<Type> ty;
        bool mut;

        NNPtr<ASTNS::AST> decl_ast() const override;

        DERIVE_DECLSYMBOL_ITEMS_DECL()

    private:
        NNPtr<ASTNS::AST> _decl_ast;

        std::map<std::string, IR::Type::Method> methods;
    };
    // Generic literal types {{{1
    // Int {{{2
    class GenericIntType : public Type {
    public:
        GenericIntType(CodeGen::Context &context, NNPtr<ASTNS::AST> decl_ast);

        std::string name() const override;

        DERIVE_TYPE_DECL()

        NNPtr<ASTNS::AST> decl_ast() const override;

        DERIVE_DECLSYMBOL_ITEMS_DECL()

    private:
        NNPtr<ASTNS::AST> _decl_ast;

        std::map<std::string, IR::Type::Method> methods;
    };
    // Float {{{2
    class GenericFloatType : public Type {
    public:
        GenericFloatType(CodeGen::Context &context, NNPtr<ASTNS::AST> decl_ast);

        std::string name() const override;

        DERIVE_TYPE_DECL()

        NNPtr<ASTNS::AST> decl_ast() const override;

        DERIVE_DECLSYMBOL_ITEMS_DECL()

    private:
        NNPtr<ASTNS::AST> _decl_ast;

        std::map<std::string, IR::Type::Method> methods;
    };
    // }}}1

    class TypeVisitor {
    public:
        virtual ~TypeVisitor() {}
#define VISITTY(cl) virtual void type_visit(cl &ty) = 0;
        IR_TYPES(VISITTY)
#undef VISITTY
    };
}

std::ostream& operator<<(std::ostream &os, NNPtr<IR::Type> const &t);
std::ostream& operator<<(std::ostream &os, NNPtr<IR::Type const> const &t);
