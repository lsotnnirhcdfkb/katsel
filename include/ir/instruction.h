#pragma once

#include <vector>
#include "ir/value.h"
#include "ir/instructionfwd.h"
#include "utils/location.h"

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
        };

        class Br {
        public:
            virtual ~Br() {};
            virtual void accept(BrVisitor &v) const = 0;
        };

        // INSTR CLASSES START
        class Copy : public Instruction {
        public:
            Copy(IR::Register& target, Located<NNPtr<Value>> val);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            Located<NNPtr<Value>> val;
        };
        class Or : public Instruction {
        public:
            Or(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class And : public Instruction {
        public:
            And(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class Not : public Instruction {
        public:
            Not(IR::Register& target, Located<NNPtr<Value>> op);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            Located<NNPtr<Value>> op;
        };
        class ICmpNE : public Instruction {
        public:
            ICmpNE(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class ICmpEQ : public Instruction {
        public:
            ICmpEQ(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class ICmpLT : public Instruction {
        public:
            ICmpLT(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class ICmpGT : public Instruction {
        public:
            ICmpGT(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class ICmpLE : public Instruction {
        public:
            ICmpLE(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class ICmpGE : public Instruction {
        public:
            ICmpGE(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class IAdd : public Instruction {
        public:
            IAdd(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class ISub : public Instruction {
        public:
            ISub(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class IMult : public Instruction {
        public:
            IMult(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class IDiv : public Instruction {
        public:
            IDiv(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class IMod : public Instruction {
        public:
            IMod(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class INeg : public Instruction {
        public:
            INeg(IR::Register& target, Located<NNPtr<Value>> op);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            Located<NNPtr<Value>> op;
        };
        class FCmpNE : public Instruction {
        public:
            FCmpNE(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class FCmpEQ : public Instruction {
        public:
            FCmpEQ(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class FCmpLT : public Instruction {
        public:
            FCmpLT(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class FCmpGT : public Instruction {
        public:
            FCmpGT(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class FCmpLE : public Instruction {
        public:
            FCmpLE(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class FCmpGE : public Instruction {
        public:
            FCmpGE(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class FAdd : public Instruction {
        public:
            FAdd(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class FSub : public Instruction {
        public:
            FSub(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class FMult : public Instruction {
        public:
            FMult(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class FDiv : public Instruction {
        public:
            FDiv(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class FMod : public Instruction {
        public:
            FMod(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class FNeg : public Instruction {
        public:
            FNeg(IR::Register& target, Located<NNPtr<Value>> op);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            Located<NNPtr<Value>> op;
        };
        class BitXor : public Instruction {
        public:
            BitXor(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class BitOr : public Instruction {
        public:
            BitOr(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class BitAnd : public Instruction {
        public:
            BitAnd(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class BitNot : public Instruction {
        public:
            BitNot(IR::Register& target, Located<NNPtr<Value>> op);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            Located<NNPtr<Value>> op;
        };
        class ShiftR : public Instruction {
        public:
            ShiftR(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class ShiftL : public Instruction {
        public:
            ShiftL(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class NoOpCast : public Instruction {
        public:
            NoOpCast(IR::Register& target, Located<NNPtr<Value>> op, NNPtr<Type const> newt);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            Located<NNPtr<Value>> op;
            NNPtr<Type const> newt;
        };
        class IntToInt : public Instruction {
        public:
            IntToInt(IR::Register& target, Located<NNPtr<Value>> op, NNPtr<IntType const> newt);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            Located<NNPtr<Value>> op;
            NNPtr<IntType const> newt;
        };
        class IntToFloat : public Instruction {
        public:
            IntToFloat(IR::Register& target, Located<NNPtr<Value>> op, NNPtr<FloatType const> newt);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            Located<NNPtr<Value>> op;
            NNPtr<FloatType const> newt;
        };
        class FloatToFloat : public Instruction {
        public:
            FloatToFloat(IR::Register& target, Located<NNPtr<Value>> op, NNPtr<FloatType const> newt);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            Located<NNPtr<Value>> op;
            NNPtr<FloatType const> newt;
        };
        class FloatToInt : public Instruction {
        public:
            FloatToInt(IR::Register& target, Located<NNPtr<Value>> op, NNPtr<IntType const> newt);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            Located<NNPtr<Value>> op;
            NNPtr<IntType const> newt;
        };
        class Call : public Instruction {
        public:
            Call(IR::Register& target, NNPtr<Function const> f, std::vector<Located<NNPtr<Value>>> args);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            NNPtr<Function const> f;
            std::vector<Located<NNPtr<Value>>> args;
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
            DerefPtr(IR::Register& target, Located<NNPtr<Value>> ptr);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            Located<NNPtr<Value>> ptr;
        };
        class PtrArith : public Instruction {
        public:
            PtrArith(IR::Register& target, Located<NNPtr<Value>> ptr, Located<NNPtr<Value>> offset);
            void accept(InstructionVisitor &v) const override;
            IR::Register& target;
            Located<NNPtr<Value>> ptr;
            Located<NNPtr<Value>> offset;
        };
        class Return : public Br {
        public:
            Return(Located<NNPtr<Value>> value);
            void accept(BrVisitor &v) const override;
            Located<NNPtr<Value>> value;
        };
        class GotoBr : public Br {
        public:
            GotoBr(NNPtr<Block> to);
            void accept(BrVisitor &v) const override;
            NNPtr<Block> to;
        };
        class CondBr : public Br {
        public:
            CondBr(Located<NNPtr<Value>> v, NNPtr<Block> true_b, NNPtr<Block> false_b);
            void accept(BrVisitor &v) const override;
            Located<NNPtr<Value>> v;
            NNPtr<Block> true_b;
            NNPtr<Block> false_b;
        };
        // INSTR CLASSES END
    }
}
