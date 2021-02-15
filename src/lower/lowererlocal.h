#pragma once

#include "lower/lowerer.h"
#include "ir/visitor.h"
#include "ir/value.h"
#include "ir/module.h"

namespace Lower {
    class LowerFunction;

    class LowerValueRef : public IR::ValueVisitor {
    public:
        LowerValueRef(LowerFunction &fl);

        llvm::Value &lower(IR::Value const &v);
    private:
#define METHOD(name) void value_visit(IR::name const &i) override;
        IR_VALUE_LIST(METHOD)
#undef METHOD

        LowerFunction &fl;

        llvm::Value *res;
    };

    class LowerInstr : public IR::InstructionVisitor, public IR::BrVisitor {
    public:
        LowerInstr(LowerFunction &fl);

        // LOWER VISIT INSTR START
        void instr_visit(IR::Instrs::Copy const &i) override;
        void instr_visit(IR::Instrs::Or const &i) override;
        void instr_visit(IR::Instrs::And const &i) override;
        void instr_visit(IR::Instrs::Not const &i) override;
        void instr_visit(IR::Instrs::ICmpNE const &i) override;
        void instr_visit(IR::Instrs::ICmpEQ const &i) override;
        void instr_visit(IR::Instrs::ICmpLT const &i) override;
        void instr_visit(IR::Instrs::ICmpGT const &i) override;
        void instr_visit(IR::Instrs::ICmpLE const &i) override;
        void instr_visit(IR::Instrs::ICmpGE const &i) override;
        void instr_visit(IR::Instrs::IAdd const &i) override;
        void instr_visit(IR::Instrs::ISub const &i) override;
        void instr_visit(IR::Instrs::IMult const &i) override;
        void instr_visit(IR::Instrs::IDiv const &i) override;
        void instr_visit(IR::Instrs::IMod const &i) override;
        void instr_visit(IR::Instrs::INeg const &i) override;
        void instr_visit(IR::Instrs::FCmpNE const &i) override;
        void instr_visit(IR::Instrs::FCmpEQ const &i) override;
        void instr_visit(IR::Instrs::FCmpLT const &i) override;
        void instr_visit(IR::Instrs::FCmpGT const &i) override;
        void instr_visit(IR::Instrs::FCmpLE const &i) override;
        void instr_visit(IR::Instrs::FCmpGE const &i) override;
        void instr_visit(IR::Instrs::FAdd const &i) override;
        void instr_visit(IR::Instrs::FSub const &i) override;
        void instr_visit(IR::Instrs::FMult const &i) override;
        void instr_visit(IR::Instrs::FDiv const &i) override;
        void instr_visit(IR::Instrs::FMod const &i) override;
        void instr_visit(IR::Instrs::FNeg const &i) override;
        void instr_visit(IR::Instrs::BitXor const &i) override;
        void instr_visit(IR::Instrs::BitOr const &i) override;
        void instr_visit(IR::Instrs::BitAnd const &i) override;
        void instr_visit(IR::Instrs::BitNot const &i) override;
        void instr_visit(IR::Instrs::ShiftR const &i) override;
        void instr_visit(IR::Instrs::ShiftL const &i) override;
        void instr_visit(IR::Instrs::NoOpCast const &i) override;
        void instr_visit(IR::Instrs::IntToInt const &i) override;
        void instr_visit(IR::Instrs::IntToFloat const &i) override;
        void instr_visit(IR::Instrs::FloatToFloat const &i) override;
        void instr_visit(IR::Instrs::FloatToInt const &i) override;
        void instr_visit(IR::Instrs::Call const &i) override;
        void instr_visit(IR::Instrs::Addrof const &i) override;
        void instr_visit(IR::Instrs::DerefPtr const &i) override;
        void instr_visit(IR::Instrs::PtrArith const &i) override;
        // LOWER VISIT INSTR END
        // LOWER VISIT BRANCH START
        void instr_visit(IR::Instrs::Return const &i) override;
        void instr_visit(IR::Instrs::GotoBr const &i) override;
        void instr_visit(IR::Instrs::CondBr const &i) override;
        // LOWER VISIT BRANCH END

        LowerFunction &fl;
    };

    class LowerValueDef : public IR::ValueVisitor {
    public:
        LowerValueDef(Lowerer &lowerer);

#define METHOD(name) void value_visit(IR::name const &i) override;
        IR_VALUE_LIST(METHOD)
#undef METHOD

    private:
        Lowerer &lowerer;
    };

    class LowerDeclSym : public IR::DeclSymbolVisitor {
    public:
        LowerDeclSym(Lowerer &lowerer);

#define METHOD(name) void declsym_visit(IR::name const &i) override;
        DECLSYM_CLASS_LIST(METHOD)
#undef METHOD

        void walk(IR::DeclSymbol const &ds);

        Lowerer &lowerer;
    };

    class LowerFunction {
    public:
        LowerFunction(Lowerer &lowerer, IR::Function const &fun);
        Lowerer &lowerer;
        LowerValueRef value_ref;
        LowerInstr instr;

        llvm::AllocaInst &get_register(IR::Register const &reg);
        llvm::BasicBlock &get_block(IR::Block const &block);
        llvm::Value &get_instruction(IR::Instruction const &instr);

        void lower();

        llvm::IRBuilder<> builder;
    private:
        IR::Function const &fun;

        llvm::BasicBlock *entry_block;

        std::unordered_map<NNPtr<IR::Register const>, llvm::AllocaInst *> registers;
        std::unordered_map<NNPtr<IR::Instruction const>, llvm::Value *> instructions;
        std::unordered_map<NNPtr<IR::Block const>, llvm::BasicBlock *> blocks;
    };
}
