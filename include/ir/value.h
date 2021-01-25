#pragma once

#include <string>
#include <cstdint>
#include <vector>
#include <variant>

#include "ast/astfwd.h"
#include "utils/ptr.h"

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

    namespace Instrs { class Instruction; }

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

        virtual Type const &type() const = 0;

        virtual void value_accept(ValueVisitor &v) const = 0;
    };

    class DeclaredValue {
    public:
        virtual ASTNS::AST const &def_ast() const = 0;
    };

    class Function;
    // Const values {{{
    class ConstInt : public Value {
    public:
        ConstInt(NNPtr<IntType> ty, uint64_t val);
        ConstInt(NNPtr<GenericIntType> ty, uint64_t val);
        Type const &type() const override;
        uint64_t val;
        void value_accept(ValueVisitor &v) const override;
    private:
        std::variant<NNPtr<IntType>, NNPtr<GenericIntType>> ty;
        bool is_generic;
    };
    class ConstFloat : public Value {
    public:
        ConstFloat(NNPtr<FloatType> ty, double val);
        ConstFloat(NNPtr<GenericFloatType> ty, double val);
        Type const &type() const override;
        double val;
        void value_accept(ValueVisitor &v) const override;
    private:
        std::variant<NNPtr<FloatType>, NNPtr<GenericFloatType>> ty;
        bool is_generic;
    };
    class ConstBool : public Value {
    public:
        ConstBool(NNPtr<BoolType> ty, bool val);
        Type const &type() const override;
        bool val;
        void value_accept(ValueVisitor &v) const override;
    private:
        NNPtr<BoolType> ty;
    };
    class ConstChar : public Value {
    public:
        ConstChar(NNPtr<CharType> ty, uint8_t val);
        Type const &type() const override;
        uint8_t val;
        void value_accept(ValueVisitor &v) const override;
    private:
        NNPtr<CharType> ty;
    };
    // }}}
    // Void {{{
    class Void : public Value {
    public:
        Void(NNPtr<VoidType> ty);
        Type const &type() const override;
        void value_accept(ValueVisitor &v) const override;
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
        NNPtr<Value const> val;
        NNPtr<ASTNS::AST const> ast;

        inline ASTValue(Value const &val, ASTNS::AST const &ast): val(val), ast(ast) {}

        inline Type const &type() const {
            return val->type();
        }
    };

    class ValueVisitor {
    public:
#define VISITMETHOD(cl, n) \
        virtual void value_visit(cl const &i) = 0;
        IR_VALUE_LIST(VISITMETHOD)
#undef VISITMETHOD
    };
}
