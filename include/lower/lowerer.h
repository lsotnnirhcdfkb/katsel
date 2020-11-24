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
        void visitIntCmpNE(IR::Instrs::IntCmpNE *instr) override;
        void visitIntCmpEQ(IR::Instrs::IntCmpEQ *instr) override;
        void visitIntCmpULT(IR::Instrs::IntCmpULT *instr) override;
        void visitIntCmpUGT(IR::Instrs::IntCmpUGT *instr) override;
        void visitIntCmpULE(IR::Instrs::IntCmpULE *instr) override;
        void visitIntCmpUGE(IR::Instrs::IntCmpUGE *instr) override;
        void visitFloatCmpNE(IR::Instrs::FloatCmpNE *instr) override;
        void visitFloatCmpEQ(IR::Instrs::FloatCmpEQ *instr) override;
        void visitFloatCmpULT(IR::Instrs::FloatCmpULT *instr) override;
        void visitFloatCmpUGT(IR::Instrs::FloatCmpUGT *instr) override;
        void visitFloatCmpULE(IR::Instrs::FloatCmpULE *instr) override;
        void visitFloatCmpUGE(IR::Instrs::FloatCmpUGE *instr) override;
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
        void visitZeroExt(IR::Instrs::ZeroExt *instr) override;
        void visitSignExt(IR::Instrs::SignExt *instr) override;
        void visitFloatTrunc(IR::Instrs::FloatTrunc *instr) override;
        void visitFloatExt(IR::Instrs::FloatExt *instr) override;
        void visitSIntToFloat(IR::Instrs::SIntToFloat *instr) override;
        void visitUIntToFloat(IR::Instrs::UIntToFloat *instr) override;
        void visitFloatToSInt(IR::Instrs::FloatToSInt *instr) override;
        void visitFloatToUInt(IR::Instrs::FloatToUInt *instr) override;
        void visitReturn(IR::Instrs::Return *instr) override;
        void visitCall(IR::Instrs::Call *instr) override;
        void visitGotoBr(IR::Instrs::GotoBr *instr) override;
        void visitCondBr(IR::Instrs::CondBr *instr) override;

        void lower();

        void printMod(std::ostream &ostream);

    private:
        void lower(IR::Function const &f);
        void lower(IR::Block const &b);

        IR::Unit const &unit;

        llvm::LLVMContext context;
        llvm::IRBuilder<> builder;
        llvm::Module mod;

        std::map<IR::Register*, llvm::AllocaInst*> allocas;
        std::map<IR::Block*, llvm::BasicBlock*> blocks;
    };
}
