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

    namespace Instrs {
        // INSTR CLASSES START
        class Copy : public Instruction {
        public:
            Copy(IR::Register& target, Located<NNPtr<Value>> val);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            IR::Register& target;
            Located<NNPtr<Value>> val;
        };
        class Or : public Instruction {
        public:
            Or(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class And : public Instruction {
        public:
            And(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class Not : public Instruction {
        public:
            Not(Located<NNPtr<Value>> op);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            Located<NNPtr<Value>> op;
        };
        class ICmpNE : public Instruction {
        public:
            ICmpNE(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class ICmpEQ : public Instruction {
        public:
            ICmpEQ(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class ICmpLT : public Instruction {
        public:
            ICmpLT(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class ICmpGT : public Instruction {
        public:
            ICmpGT(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class ICmpLE : public Instruction {
        public:
            ICmpLE(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class ICmpGE : public Instruction {
        public:
            ICmpGE(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class IAdd : public Instruction {
        public:
            IAdd(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class ISub : public Instruction {
        public:
            ISub(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class IMult : public Instruction {
        public:
            IMult(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class IDiv : public Instruction {
        public:
            IDiv(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class IMod : public Instruction {
        public:
            IMod(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class INeg : public Instruction {
        public:
            INeg(Located<NNPtr<Value>> op);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            Located<NNPtr<Value>> op;
        };
        class FCmpNE : public Instruction {
        public:
            FCmpNE(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class FCmpEQ : public Instruction {
        public:
            FCmpEQ(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class FCmpLT : public Instruction {
        public:
            FCmpLT(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class FCmpGT : public Instruction {
        public:
            FCmpGT(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class FCmpLE : public Instruction {
        public:
            FCmpLE(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class FCmpGE : public Instruction {
        public:
            FCmpGE(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class FAdd : public Instruction {
        public:
            FAdd(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class FSub : public Instruction {
        public:
            FSub(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class FMult : public Instruction {
        public:
            FMult(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class FDiv : public Instruction {
        public:
            FDiv(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class FMod : public Instruction {
        public:
            FMod(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class FNeg : public Instruction {
        public:
            FNeg(Located<NNPtr<Value>> op);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            Located<NNPtr<Value>> op;
        };
        class BitXor : public Instruction {
        public:
            BitXor(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class BitOr : public Instruction {
        public:
            BitOr(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class BitAnd : public Instruction {
        public:
            BitAnd(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class BitNot : public Instruction {
        public:
            BitNot(Located<NNPtr<Value>> op);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            Located<NNPtr<Value>> op;
        };
        class ShiftR : public Instruction {
        public:
            ShiftR(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class ShiftL : public Instruction {
        public:
            ShiftL(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            Located<NNPtr<Value>> lhs;
            Located<NNPtr<Value>> rhs;
        };
        class NoOpCast : public Instruction {
        public:
            NoOpCast(Located<NNPtr<Value>> op, NNPtr<Type const> newt);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            Located<NNPtr<Value>> op;
            NNPtr<Type const> newt;
        };
        class IntToInt : public Instruction {
        public:
            IntToInt(Located<NNPtr<Value>> op, NNPtr<IntType const> newt);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            Located<NNPtr<Value>> op;
            NNPtr<IntType const> newt;
        };
        class IntToFloat : public Instruction {
        public:
            IntToFloat(Located<NNPtr<Value>> op, NNPtr<FloatType const> newt);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            Located<NNPtr<Value>> op;
            NNPtr<FloatType const> newt;
        };
        class FloatToFloat : public Instruction {
        public:
            FloatToFloat(Located<NNPtr<Value>> op, NNPtr<FloatType const> newt);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            Located<NNPtr<Value>> op;
            NNPtr<FloatType const> newt;
        };
        class FloatToInt : public Instruction {
        public:
            FloatToInt(Located<NNPtr<Value>> op, NNPtr<IntType const> newt);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            Located<NNPtr<Value>> op;
            NNPtr<IntType const> newt;
        };
        class Call : public Instruction {
        public:
            Call(NNPtr<Function const> f, std::vector<Located<NNPtr<Value>>> args);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            NNPtr<Function const> f;
            std::vector<Located<NNPtr<Value>>> args;
        };
        class Addrof : public Instruction {
        public:
            Addrof(IR::Register& reg, bool mut);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            IR::Register& reg;
            bool mut;
        };
        class DerefPtr : public Instruction {
        public:
            DerefPtr(Located<NNPtr<Value>> ptr);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
            Located<NNPtr<Value>> ptr;
        };
        class PtrArith : public Instruction {
        public:
            PtrArith(Located<NNPtr<Value>> ptr, Located<NNPtr<Value>> offset);
            void accept(InstructionVisitor &v) const override;
            IR::Type const &type() const override;
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
