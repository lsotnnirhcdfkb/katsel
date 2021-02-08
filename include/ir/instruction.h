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
        class Instruction {
        public:
            virtual ~Instruction() {};
            virtual void accept(InstructionVisitor &v) const = 0;

            size_t id;
        };

        class Br {
        public:
            virtual ~Br() {};
            virtual void accept(BrVisitor &v) const = 0;
        };

        // INSTR CLASSES START
        class Copy : public Instruction {
        public:
            Copy(IR::Register& target, ASTValue val);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            ASTValue val;
        };
        class Or : public Instruction {
        public:
            Or(IR::Register& target, ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            ASTValue lhs;
            ASTValue rhs;
        };
        class And : public Instruction {
        public:
            And(IR::Register& target, ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            ASTValue lhs;
            ASTValue rhs;
        };
        class Not : public Instruction {
        public:
            Not(IR::Register& target, ASTValue op);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            ASTValue op;
        };
        class ICmpNE : public Instruction {
        public:
            ICmpNE(IR::Register& target, ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            ASTValue lhs;
            ASTValue rhs;
        };
        class ICmpEQ : public Instruction {
        public:
            ICmpEQ(IR::Register& target, ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            ASTValue lhs;
            ASTValue rhs;
        };
        class ICmpLT : public Instruction {
        public:
            ICmpLT(IR::Register& target, ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            ASTValue lhs;
            ASTValue rhs;
        };
        class ICmpGT : public Instruction {
        public:
            ICmpGT(IR::Register& target, ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            ASTValue lhs;
            ASTValue rhs;
        };
        class ICmpLE : public Instruction {
        public:
            ICmpLE(IR::Register& target, ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            ASTValue lhs;
            ASTValue rhs;
        };
        class ICmpGE : public Instruction {
        public:
            ICmpGE(IR::Register& target, ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            ASTValue lhs;
            ASTValue rhs;
        };
        class IAdd : public Instruction {
        public:
            IAdd(IR::Register& target, ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            ASTValue lhs;
            ASTValue rhs;
        };
        class ISub : public Instruction {
        public:
            ISub(IR::Register& target, ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            ASTValue lhs;
            ASTValue rhs;
        };
        class IMult : public Instruction {
        public:
            IMult(IR::Register& target, ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            ASTValue lhs;
            ASTValue rhs;
        };
        class IDiv : public Instruction {
        public:
            IDiv(IR::Register& target, ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            ASTValue lhs;
            ASTValue rhs;
        };
        class IMod : public Instruction {
        public:
            IMod(IR::Register& target, ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            ASTValue lhs;
            ASTValue rhs;
        };
        class INeg : public Instruction {
        public:
            INeg(IR::Register& target, ASTValue op);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            ASTValue op;
        };
        class FCmpNE : public Instruction {
        public:
            FCmpNE(IR::Register& target, ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            ASTValue lhs;
            ASTValue rhs;
        };
        class FCmpEQ : public Instruction {
        public:
            FCmpEQ(IR::Register& target, ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            ASTValue lhs;
            ASTValue rhs;
        };
        class FCmpLT : public Instruction {
        public:
            FCmpLT(IR::Register& target, ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            ASTValue lhs;
            ASTValue rhs;
        };
        class FCmpGT : public Instruction {
        public:
            FCmpGT(IR::Register& target, ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            ASTValue lhs;
            ASTValue rhs;
        };
        class FCmpLE : public Instruction {
        public:
            FCmpLE(IR::Register& target, ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            ASTValue lhs;
            ASTValue rhs;
        };
        class FCmpGE : public Instruction {
        public:
            FCmpGE(IR::Register& target, ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            ASTValue lhs;
            ASTValue rhs;
        };
        class FAdd : public Instruction {
        public:
            FAdd(IR::Register& target, ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            ASTValue lhs;
            ASTValue rhs;
        };
        class FSub : public Instruction {
        public:
            FSub(IR::Register& target, ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            ASTValue lhs;
            ASTValue rhs;
        };
        class FMult : public Instruction {
        public:
            FMult(IR::Register& target, ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            ASTValue lhs;
            ASTValue rhs;
        };
        class FDiv : public Instruction {
        public:
            FDiv(IR::Register& target, ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            ASTValue lhs;
            ASTValue rhs;
        };
        class FMod : public Instruction {
        public:
            FMod(IR::Register& target, ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            ASTValue lhs;
            ASTValue rhs;
        };
        class FNeg : public Instruction {
        public:
            FNeg(IR::Register& target, ASTValue op);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            ASTValue op;
        };
        class BitXor : public Instruction {
        public:
            BitXor(IR::Register& target, ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            ASTValue lhs;
            ASTValue rhs;
        };
        class BitOr : public Instruction {
        public:
            BitOr(IR::Register& target, ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            ASTValue lhs;
            ASTValue rhs;
        };
        class BitAnd : public Instruction {
        public:
            BitAnd(IR::Register& target, ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            ASTValue lhs;
            ASTValue rhs;
        };
        class BitNot : public Instruction {
        public:
            BitNot(IR::Register& target, ASTValue op);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            ASTValue op;
        };
        class ShiftR : public Instruction {
        public:
            ShiftR(IR::Register& target, ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            ASTValue lhs;
            ASTValue rhs;
        };
        class ShiftL : public Instruction {
        public:
            ShiftL(IR::Register& target, ASTValue lhs, ASTValue rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            ASTValue lhs;
            ASTValue rhs;
        };
        class NoOpCast : public Instruction {
        public:
            NoOpCast(IR::Register& target, ASTValue op, NNPtr<Type const> newt);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            ASTValue op;
            NNPtr<Type const> newt;
        };
        class IntToInt : public Instruction {
        public:
            IntToInt(IR::Register& target, ASTValue op, NNPtr<IntType const> newt);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            ASTValue op;
            NNPtr<IntType const> newt;
        };
        class IntToFloat : public Instruction {
        public:
            IntToFloat(IR::Register& target, ASTValue op, NNPtr<FloatType const> newt);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            ASTValue op;
            NNPtr<FloatType const> newt;
        };
        class FloatToFloat : public Instruction {
        public:
            FloatToFloat(IR::Register& target, ASTValue op, NNPtr<FloatType const> newt);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            ASTValue op;
            NNPtr<FloatType const> newt;
        };
        class FloatToInt : public Instruction {
        public:
            FloatToInt(IR::Register& target, ASTValue op, NNPtr<IntType const> newt);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            ASTValue op;
            NNPtr<IntType const> newt;
        };
        class Call : public Instruction {
        public:
            Call(IR::Register& target, NNPtr<Function const> f, std::vector<ASTValue> args);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            NNPtr<Function const> f;
            std::vector<ASTValue> args;
        };
        class Addrof : public Instruction {
        public:
            Addrof(IR::Register& target, IR::Register& reg, bool mut);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            IR::Register& reg;
            bool mut;
        };
        class DerefPtr : public Instruction {
        public:
            DerefPtr(IR::Register& target, ASTValue ptr);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            ASTValue ptr;
        };
        class PtrArith : public Instruction {
        public:
            PtrArith(IR::Register& target, ASTValue ptr, ASTValue offset);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
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
