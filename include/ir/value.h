#pragma once

#include "ir/block.h"
#include <string>
#include <cstddef>
#include <cstdint>
#include <vector>
#include <variant>
#include "ast/astfwd.h"

namespace IR {
    class Type;
    class FloatType;
    class IntType;
    class GenericFloatType;
    class GenericIntType;
    class CharType;
    class BoolType;
    class FunctionType;
    class VoidType;

#define IR_VALUE_LIST(macro) \
    macro(ConstBool, ConstBool) \
    macro(ConstChar, ConstChar) \
    macro(ConstInt, ConstInt) \
    macro(ConstFloat, ConstFloat) \
    macro(Function, Function) \
    macro(Instrs::Instruction, Instruction) \
    macro(Void, Void)

    class ValueVisitor;
    class Value {
    public:
        virtual ~Value() {};

        virtual NNPtr<Type> type() const = 0;

        virtual void value_accept(ValueVisitor &v) = 0;
    };

    class DeclaredValue {
    public:
        virtual NNPtr<ASTNS::AST> defAST() const = 0;
    };

    // Function {{{
    class Function : public Value, public DeclaredValue {
    public:
        Function(NNPtr<FunctionType> ty, std::string name, NNPtr<ASTNS::FunctionDecl> defAST);

        void add(std::unique_ptr<Block> block);

        void definition(llvm::raw_ostream &os) const;
        NNPtr<ASTNS::AST> defAST() const override;

        NNPtr<Type> type() const override;

        std::vector<std::unique_ptr<Block>> blocks;

        NNPtr<Block> addBlock(std::string name);

        NNPtr<FunctionType> ty;
        std::string name;

        bool prototypeonly;

        void value_accept(ValueVisitor &v) override;

        uint64_t curindex;

        NNPtr<ASTNS::FunctionDecl> _defAST;

    private:
        uint64_t blocki;
    };
    // }}}
    // Const values {{{
    class ConstInt : public Value {
    public:
        ConstInt(NNPtr<IntType> ty, uint64_t val);
        ConstInt(NNPtr<GenericIntType> ty, uint64_t val);
        NNPtr<Type> type() const override;
        uint64_t val;
        void value_accept(ValueVisitor &v) override;
    private:
        std::variant<NNPtr<IntType>, NNPtr<GenericIntType>> ty;
        bool isGeneric;
    };
    class ConstFloat : public Value {
    public:
        ConstFloat(NNPtr<FloatType> ty, double val);
        ConstFloat(NNPtr<GenericFloatType> ty, double val);
        NNPtr<Type> type() const override;
        double val;
        void value_accept(ValueVisitor &v) override;
    private:
        std::variant<NNPtr<FloatType>, NNPtr<GenericFloatType>> ty;
        bool isGeneric;
    };
    class ConstBool : public Value {
    public:
        ConstBool(NNPtr<BoolType> ty, bool val);
        NNPtr<Type> type() const override;
        bool val;
        void value_accept(ValueVisitor &v) override;
    private:
        NNPtr<BoolType> ty;
    };
    class ConstChar : public Value {
    public:
        ConstChar(NNPtr<CharType> ty, uint8_t val);
        NNPtr<Type> type() const override;
        uint8_t val;
        void value_accept(ValueVisitor &v) override;
    private:
        NNPtr<CharType> ty;
    };
    // }}}
    // Void {{{
    class Void : public Value {
    public:
        Void(NNPtr<VoidType> ty);
        NNPtr<Type> type() const override;
        void value_accept(ValueVisitor &v) override;
    private:
        NNPtr<VoidType> ty;
    };
    // }}}

    struct ASTValue {
        // Because multiple ASTs can evaluate to the same value (same pointer)
        // (see primary expressions (multiple PrimaryASTs can evaluate to the same register),
        // also especially since ConstInts are uniqued together),
        // this struct allows values to be associated with an ast, and more importantly,
        // it isn't supposed to be heap-allocated and uniqued, so one value can have multiple ASTs
        NNPtr<Value> val;
        NNPtr<ASTNS::AST> ast;

        inline ASTValue(NNPtr<Value> val, NNPtr<ASTNS::AST> ast): val(val), ast(ast) {}

        inline NNPtr<Type> type() const {
            return val->type();
        }
    };

    class ValueVisitor {
    public:
#define VISITMETHOD(cl, n) \
        virtual void value_visit##n(cl &i) = 0;
        IR_VALUE_LIST(VISITMETHOD)
#undef VISITMETHOD
    };
}
