#pragma once

#include "ir/instructionfwd.h"

namespace IR {
    class InstructionVisitor {
    public:
        virtual ~InstructionVisitor() {}

        // PURE INSTR VISIT START
        virtual void visit(IR::Instrs::Copy const &i) = 0;
        virtual void visit(IR::Instrs::Or const &i) = 0;
        virtual void visit(IR::Instrs::And const &i) = 0;
        virtual void visit(IR::Instrs::Not const &i) = 0;
        virtual void visit(IR::Instrs::ICmpNE const &i) = 0;
        virtual void visit(IR::Instrs::ICmpEQ const &i) = 0;
        virtual void visit(IR::Instrs::ICmpLT const &i) = 0;
        virtual void visit(IR::Instrs::ICmpGT const &i) = 0;
        virtual void visit(IR::Instrs::ICmpLE const &i) = 0;
        virtual void visit(IR::Instrs::ICmpGE const &i) = 0;
        virtual void visit(IR::Instrs::IAdd const &i) = 0;
        virtual void visit(IR::Instrs::ISub const &i) = 0;
        virtual void visit(IR::Instrs::IMult const &i) = 0;
        virtual void visit(IR::Instrs::IDiv const &i) = 0;
        virtual void visit(IR::Instrs::IMod const &i) = 0;
        virtual void visit(IR::Instrs::INeg const &i) = 0;
        virtual void visit(IR::Instrs::FCmpNE const &i) = 0;
        virtual void visit(IR::Instrs::FCmpEQ const &i) = 0;
        virtual void visit(IR::Instrs::FCmpLT const &i) = 0;
        virtual void visit(IR::Instrs::FCmpGT const &i) = 0;
        virtual void visit(IR::Instrs::FCmpLE const &i) = 0;
        virtual void visit(IR::Instrs::FCmpGE const &i) = 0;
        virtual void visit(IR::Instrs::FAdd const &i) = 0;
        virtual void visit(IR::Instrs::FSub const &i) = 0;
        virtual void visit(IR::Instrs::FMult const &i) = 0;
        virtual void visit(IR::Instrs::FDiv const &i) = 0;
        virtual void visit(IR::Instrs::FMod const &i) = 0;
        virtual void visit(IR::Instrs::FNeg const &i) = 0;
        virtual void visit(IR::Instrs::BitXor const &i) = 0;
        virtual void visit(IR::Instrs::BitOr const &i) = 0;
        virtual void visit(IR::Instrs::BitAnd const &i) = 0;
        virtual void visit(IR::Instrs::BitNot const &i) = 0;
        virtual void visit(IR::Instrs::ShiftR const &i) = 0;
        virtual void visit(IR::Instrs::ShiftL const &i) = 0;
        virtual void visit(IR::Instrs::NoOpCast const &i) = 0;
        virtual void visit(IR::Instrs::IntToInt const &i) = 0;
        virtual void visit(IR::Instrs::IntToFloat const &i) = 0;
        virtual void visit(IR::Instrs::FloatToFloat const &i) = 0;
        virtual void visit(IR::Instrs::FloatToInt const &i) = 0;
        virtual void visit(IR::Instrs::Call const &i) = 0;
        virtual void visit(IR::Instrs::Addrof const &i) = 0;
        virtual void visit(IR::Instrs::DerefPtr const &i) = 0;
        virtual void visit(IR::Instrs::PtrArith const &i) = 0;
        // PURE INSTR VISIT END
    };

    class BrVisitor {
    public:
        virtual ~BrVisitor() {}

        // PURE BRANCH VISIT START
        virtual void visit(IR::Instrs::Return const &i) = 0;
        virtual void visit(IR::Instrs::GotoBr const &i) = 0;
        virtual void visit(IR::Instrs::CondBr const &i) = 0;
        // PURE BRANCH VISIT END
    };
}
