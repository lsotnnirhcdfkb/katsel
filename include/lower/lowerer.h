#pragma once

#include "ir/instruction.h"
#include "ir/value.h"
#include "ir/unit.h"
#include "ir/visitor.h"

#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"

namespace Lower
{
    class Lowerer : public IR::InstructionVisitor, public IR::BrVisitor
    {
    public:
        Lowerer(IR::Unit const &unit);

        void visitStore(IR::Instrs::Store *instr) override;
        void visitOr(IR::Instrs::Or *instr) override;
        void visitAnd(IR::Instrs::And *instr) override;
        void visitCmpNE(IR::Instrs::CmpNE *instr) override;
        void visitCmpEQ(IR::Instrs::CmpEQ *instr) override;
        void visitCmpLT(IR::Instrs::CmpLT *instr) override;
        void visitCmpGT(IR::Instrs::CmpGT *instr) override;
        void visitCmpLE(IR::Instrs::CmpLE *instr) override;
        void visitCmpGE(IR::Instrs::CmpGE *instr) override;
        void visitBitXor(IR::Instrs::BitXor *instr) override;
        void visitBitOr(IR::Instrs::BitOr *instr) override;
        void visitBitAnd(IR::Instrs::BitAnd *instr) override;
        void visitBitNot(IR::Instrs::BitNot *instr) override;
        void visitShiftR(IR::Instrs::ShiftR *instr) override;
        void visitShiftL(IR::Instrs::ShiftL *instr) override;
        void visitAdd(IR::Instrs::Add *instr) override;
        void visitSub(IR::Instrs::Sub *instr) override;
        void visitMult(IR::Instrs::Mult *instr) override;
        void visitDiv(IR::Instrs::Div *instr) override;
        void visitMod(IR::Instrs::Mod *instr) override;
        void visitNeg(IR::Instrs::Neg *instr) override;
        void visitTrunc(IR::Instrs::Trunc *instr) override;
        void visitExt(IR::Instrs::Ext *instr) override;
        void visitIntToFloat(IR::Instrs::IntToFloat *instr) override;
        void visitFloatToInt(IR::Instrs::FloatToInt *instr) override;
        void visitReturn(IR::Instrs::Return *instr) override;
        void visitCall(IR::Instrs::Call *instr) override;

        void visitGotoBr(IR::Instrs::GotoBr *instr) override;
        void visitCondBr(IR::Instrs::CondBr *instr) override;

        void lower();

        void printMod(std::ostream &ostream);

    private:
        void lower(IR::Function const &f);
        void lower(IR::Block const &b);
        llvm::Value* lower(IR::Value const *v);

        IR::Unit const &unit;

        llvm::LLVMContext context;
        llvm::IRBuilder<> builder;
        llvm::Module mod;

        std::map<IR::Register const *, llvm::AllocaInst*> allocas;
        std::map<IR::Block const *, llvm::BasicBlock*> blocks;
        std::map<IR::Function const *, llvm::Function*> functions;
    };
}
