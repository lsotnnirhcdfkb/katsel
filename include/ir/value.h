#pragma once

#include "ir/block.h"
#include <string>
#include <cstddef>
#include <vector>
#include "ast/ast.h"

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
        virtual std::string stringify() const = 0;

        virtual Type* type() const = 0;

        virtual void value_accept(ValueVisitor *v) = 0;
    };

    class DeclaredValue {
    public:
        virtual ASTNS::AST* defAST() const = 0;
    };

    // Function {{{
    class Function : public Value, public DeclaredValue {
    public:
        Function(FunctionType *ty, std::string name, ASTNS::FunctionDecl *defAST);

        void add(std::unique_ptr<Block> block);

        std::string stringify() const override;
        void definition(llvm::raw_ostream &os) const;
        ASTNS::FunctionDecl* defAST() const override;
        void cfgDot(llvm::raw_ostream &os) const;

        Type* type() const override;

        std::vector<std::unique_ptr<Block>> blocks;

        Block* addBlock(std::string name);

        FunctionType *ty;
        std::string name;

        bool prototypeonly;

        void value_accept(ValueVisitor *v) override;

    private:
        ASTNS::FunctionDecl *_defAST;

        size_t blocki;
    };
    // }}}
    // Const values {{{
    class ConstInt : public Value {
    public:
        ConstInt(IntType *ty, uint64_t val);
        ConstInt(GenericIntType *ty, uint64_t val);
        std::string stringify() const override;
        Type* type() const override;
        uint64_t val;
        void value_accept(ValueVisitor *v) override;
    private:
        IntType *concreteTy;
        GenericIntType *genericTy;
        bool isGeneric;
    };
    class ConstFloat : public Value {
    public:
        ConstFloat(FloatType *ty, double val);
        ConstFloat(GenericFloatType *ty, double val);
        std::string stringify() const override;
        Type* type() const override;
        double val;
        void value_accept(ValueVisitor *v) override;
    private:
        FloatType *concreteTy;
        GenericFloatType *genericTy;
        bool isGeneric;
    };
    class ConstBool : public Value {
    public:
        ConstBool(BoolType *ty, bool val);
        std::string stringify() const override;
        Type* type() const override;
        bool val;
        void value_accept(ValueVisitor *v) override;
    private:
        BoolType *ty;
    };
    class ConstChar : public Value {
    public:
        ConstChar(CharType *ty, uint8_t val);
        std::string stringify() const override;
        Type* type() const override;
        uint8_t val;
        void value_accept(ValueVisitor *v) override;
    private:
        CharType *ty;
    };
    // }}}
    // Void {{{
    class Void : public Value {
    public:
        Void(VoidType *ty);
        std::string stringify() const override;
        Type* type() const override;
        void value_accept(ValueVisitor *v) override;
    private:
        VoidType *ty;
    };
    // }}}

    struct ASTValue {
        // Because multiple ASTs can evaluate to the same value (same pointer)
        // (see primary expressions (multiple PrimaryASTs can evaluate to the same register),
        // also especially since ConstInts are uniqued together),
        // this struct allows values to be associated with an ast, and more importantly,
        // it isn't supposed to be heap-allocated and uniqued, so one value can have multiple ASTs
        Value *val;
        ASTNS::AST *ast;

        inline ASTValue(Value *val, ASTNS::AST *ast): val(val), ast(ast) {}
        inline ASTValue(): val(nullptr), ast(nullptr) {}

        explicit inline operator bool() const {
            return val;
        }

        inline Type* type() const {
            return val->type();
        }
        inline std::string stringify() const {
            return val->stringify();
        }
    };

    class ValueVisitor {
    public:
#define VISITMETHOD(cl, n) \
        virtual void value_visit##n(cl *i) = 0;
        IR_VALUE_LIST(VISITMETHOD)
#undef VISITMETHOD
    };
}

inline std::ostream& operator<<(std::ostream& os, IR::Value *v) {
    os << v->stringify();
    return os;
}

inline std::ostream& operator<<(std::ostream &os, IR::ASTValue const &v) {
    os << v.stringify();
    return os;
}
