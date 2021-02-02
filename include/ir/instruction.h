#pragma once

#include <vector>
#include "ir/value.h"
#include "ir/instructionfwd.h"

namespace IR {
    class Type;

    class Block;

    class InstructionVisitor;
    class BrVisitor;

    namespace Instrs {
        class Instruction : public Value {
        public:
            virtual ~Instruction() {};
            virtual void accept(InstructionVisitor &v) const = 0;
            void value_accept(ValueVisitor &v) const override;

            size_t id;
        };

        class Br {
        public:
            virtual ~Br() {};
            virtual void accept(BrVisitor &v) const = 0;
        };

        // INSTR CLASSES START
        class Store : public Instruction {
        public:
            Store(ASTValue target, ASTValue value, bool init);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            ASTValue target;
            ASTValue value;
            bool init;
        };
        class Phi : public Instruction {
        public:
            Phi(std::vector<std::pair<NNPtr<Block const>, ASTValue>> prevs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            std::vector<std::pair<NNPtr<Block const>, ASTValue>> prevs;
        };
        class Register : public Instruction, public DeclaredValue {
        public:
            Register(NNPtr<ASTNS::AST> _def_ast, NNPtr<Type const> ty, bool mut);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            ASTNS::AST& def_ast() const override;
            NNPtr<ASTNS::AST> _def_ast;
            NNPtr<Type const> ty;
            bool mut;
        };
        class Or : public Instruction {
        public:
            Or(ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            ASTValue lhs;
            ASTValue rhs;
        };
        class And : public Instruction {
        public:
            And(ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            ASTValue lhs;
            ASTValue rhs;
        };
        class Not : public Instruction {
        public:
            Not(ASTValue op);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            ASTValue op;
        };
        class ICmpNE : public Instruction {
        public:
            ICmpNE(ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            ASTValue lhs;
            ASTValue rhs;
        };
        class ICmpEQ : public Instruction {
        public:
            ICmpEQ(ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            ASTValue lhs;
            ASTValue rhs;
        };
        class ICmpLT : public Instruction {
        public:
            ICmpLT(ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            ASTValue lhs;
            ASTValue rhs;
        };
        class ICmpGT : public Instruction {
        public:
            ICmpGT(ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            ASTValue lhs;
            ASTValue rhs;
        };
        class ICmpLE : public Instruction {
        public:
            ICmpLE(ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            ASTValue lhs;
            ASTValue rhs;
        };
        class ICmpGE : public Instruction {
        public:
            ICmpGE(ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            ASTValue lhs;
            ASTValue rhs;
        };
        class IAdd : public Instruction {
        public:
            IAdd(ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            ASTValue lhs;
            ASTValue rhs;
        };
        class ISub : public Instruction {
        public:
            ISub(ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            ASTValue lhs;
            ASTValue rhs;
        };
        class IMult : public Instruction {
        public:
            IMult(ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            ASTValue lhs;
            ASTValue rhs;
        };
        class IDiv : public Instruction {
        public:
            IDiv(ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            ASTValue lhs;
            ASTValue rhs;
        };
        class IMod : public Instruction {
        public:
            IMod(ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            ASTValue lhs;
            ASTValue rhs;
        };
        class INeg : public Instruction {
        public:
            INeg(ASTValue op);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            ASTValue op;
        };
        class FCmpNE : public Instruction {
        public:
            FCmpNE(ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            ASTValue lhs;
            ASTValue rhs;
        };
        class FCmpEQ : public Instruction {
        public:
            FCmpEQ(ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            ASTValue lhs;
            ASTValue rhs;
        };
        class FCmpLT : public Instruction {
        public:
            FCmpLT(ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            ASTValue lhs;
            ASTValue rhs;
        };
        class FCmpGT : public Instruction {
        public:
            FCmpGT(ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            ASTValue lhs;
            ASTValue rhs;
        };
        class FCmpLE : public Instruction {
        public:
            FCmpLE(ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            ASTValue lhs;
            ASTValue rhs;
        };
        class FCmpGE : public Instruction {
        public:
            FCmpGE(ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            ASTValue lhs;
            ASTValue rhs;
        };
        class FAdd : public Instruction {
        public:
            FAdd(ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            ASTValue lhs;
            ASTValue rhs;
        };
        class FSub : public Instruction {
        public:
            FSub(ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            ASTValue lhs;
            ASTValue rhs;
        };
        class FMult : public Instruction {
        public:
            FMult(ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            ASTValue lhs;
            ASTValue rhs;
        };
        class FDiv : public Instruction {
        public:
            FDiv(ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            ASTValue lhs;
            ASTValue rhs;
        };
        class FMod : public Instruction {
        public:
            FMod(ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            ASTValue lhs;
            ASTValue rhs;
        };
        class FNeg : public Instruction {
        public:
            FNeg(ASTValue op);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            ASTValue op;
        };
        class BitXor : public Instruction {
        public:
            BitXor(ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            ASTValue lhs;
            ASTValue rhs;
        };
        class BitOr : public Instruction {
        public:
            BitOr(ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            ASTValue lhs;
            ASTValue rhs;
        };
        class BitAnd : public Instruction {
        public:
            BitAnd(ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            ASTValue lhs;
            ASTValue rhs;
        };
        class BitNot : public Instruction {
        public:
            BitNot(ASTValue op);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            ASTValue op;
        };
        class ShiftR : public Instruction {
        public:
            ShiftR(ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            ASTValue lhs;
            ASTValue rhs;
        };
        class ShiftL : public Instruction {
        public:
            ShiftL(ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            ASTValue lhs;
            ASTValue rhs;
        };
        class NoOpCast : public Instruction {
        public:
            NoOpCast(ASTValue op, NNPtr<Type const> newt);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            ASTValue op;
            NNPtr<Type const> newt;
        };
        class IntToInt : public Instruction {
        public:
            IntToInt(ASTValue op, NNPtr<IntType const> newt);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            ASTValue op;
            NNPtr<IntType const> newt;
        };
        class IntToFloat : public Instruction {
        public:
            IntToFloat(ASTValue op, NNPtr<FloatType const> newt);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            ASTValue op;
            NNPtr<FloatType const> newt;
        };
        class FloatToFloat : public Instruction {
        public:
            FloatToFloat(ASTValue op, NNPtr<FloatType const> newt);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            ASTValue op;
            NNPtr<FloatType const> newt;
        };
        class FloatToInt : public Instruction {
        public:
            FloatToInt(ASTValue op, NNPtr<IntType const> newt);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            ASTValue op;
            NNPtr<IntType const> newt;
        };
        class Call : public Instruction {
        public:
            Call(NNPtr<Function const> f, std::vector<ASTValue> args);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            NNPtr<Function const> f;
            std::vector<ASTValue> args;
        };
        class Addrof : public Instruction {
        public:
            Addrof(NNPtr<DerefPtr const> deref, bool mut);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            NNPtr<DerefPtr const> deref;
            bool mut;
        };
        class DerefPtr : public Instruction {
        public:
            DerefPtr(ASTValue ptr);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            ASTValue ptr;
        };
        class PtrArith : public Instruction {
        public:
            PtrArith(ASTValue ptr, ASTValue offset);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            ASTValue ptr;
            ASTValue offset;
        };
        class Return : public Br {
        public:
            Return(ASTValue value);
            void accept(BrVisitor &v) const override;
            ASTValue value;
        };
        class GotoBr : public Br {
        public:
            GotoBr(NNPtr<Block> to);
            void accept(BrVisitor &v) const override;
            NNPtr<Block> to;
        };
        class CondBr : public Br {
        public:
            CondBr(ASTValue v, NNPtr<Block> true_b, NNPtr<Block> false_b);
            void accept(BrVisitor &v) const override;
            ASTValue v;
            NNPtr<Block> true_b;
            NNPtr<Block> false_b;
        };
        // INSTR CLASSES END
    }
}
