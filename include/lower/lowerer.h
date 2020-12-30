#pragma once

#include "ir/instruction.h"
#include "ir/value.h"
#include "ir/unit.h"
#include "ir/visitor.h"
#include "mangle/mangler.h"

#include "llvm/IR/Module.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"

namespace Lower
{
    class Lowerer : public IR::InstructionVisitor, public IR::BrVisitor
    {
    public:
        Lowerer(IR::Unit const &unit);

        // LOWER VISIT INSTR START
// The following code was autogenerated - see the utils/ directory
void visitStore(IR::Instrs::Store *i) override;
void visitPhi(IR::Instrs::Phi *i) override;
void visitRegister(IR::Instrs::Register *i) override;
void visitOr(IR::Instrs::Or *i) override;
void visitAnd(IR::Instrs::And *i) override;
void visitNot(IR::Instrs::Not *i) override;
void visitICmpNE(IR::Instrs::ICmpNE *i) override;
void visitICmpEQ(IR::Instrs::ICmpEQ *i) override;
void visitICmpLT(IR::Instrs::ICmpLT *i) override;
void visitICmpGT(IR::Instrs::ICmpGT *i) override;
void visitICmpLE(IR::Instrs::ICmpLE *i) override;
void visitICmpGE(IR::Instrs::ICmpGE *i) override;
void visitIAdd(IR::Instrs::IAdd *i) override;
void visitISub(IR::Instrs::ISub *i) override;
void visitIMult(IR::Instrs::IMult *i) override;
void visitIDiv(IR::Instrs::IDiv *i) override;
void visitIMod(IR::Instrs::IMod *i) override;
void visitINeg(IR::Instrs::INeg *i) override;
void visitFCmpNE(IR::Instrs::FCmpNE *i) override;
void visitFCmpEQ(IR::Instrs::FCmpEQ *i) override;
void visitFCmpLT(IR::Instrs::FCmpLT *i) override;
void visitFCmpGT(IR::Instrs::FCmpGT *i) override;
void visitFCmpLE(IR::Instrs::FCmpLE *i) override;
void visitFCmpGE(IR::Instrs::FCmpGE *i) override;
void visitFAdd(IR::Instrs::FAdd *i) override;
void visitFSub(IR::Instrs::FSub *i) override;
void visitFMult(IR::Instrs::FMult *i) override;
void visitFDiv(IR::Instrs::FDiv *i) override;
void visitFMod(IR::Instrs::FMod *i) override;
void visitFNeg(IR::Instrs::FNeg *i) override;
void visitBitXor(IR::Instrs::BitXor *i) override;
void visitBitOr(IR::Instrs::BitOr *i) override;
void visitBitAnd(IR::Instrs::BitAnd *i) override;
void visitBitNot(IR::Instrs::BitNot *i) override;
void visitShiftR(IR::Instrs::ShiftR *i) override;
void visitShiftL(IR::Instrs::ShiftL *i) override;
void visitNoOpCast(IR::Instrs::NoOpCast *i) override;
void visitIntToInt(IR::Instrs::IntToInt *i) override;
void visitIntToFloat(IR::Instrs::IntToFloat *i) override;
void visitFloatToFloat(IR::Instrs::FloatToFloat *i) override;
void visitFloatToInt(IR::Instrs::FloatToInt *i) override;
void visitCall(IR::Instrs::Call *i) override;
void visitDerefPtr(IR::Instrs::DerefPtr *i) override;
// This code was autogenerated - see the utils/ directory
        // LOWER VISIT INSTR END
        // LOWER VISIT BRANCH START
// The following code was autogenerated - see the utils/ directory
void visitReturn(IR::Instrs::Return *i) override;
void visitGotoBr(IR::Instrs::GotoBr *i) override;
void visitCondBr(IR::Instrs::CondBr *i) override;
// This code was autogenerated - see the utils/ directory
        // LOWER VISIT BRANCH END

        void lower();
        bool objectify(llvm::raw_fd_ostream &ostream);

        void printMod(llvm::raw_ostream &ostream);

        bool errored;

    private:
        void lower(IR::Function const &f);
        void lower(IR::Block const &b);
        llvm::Value* lower(IR::Value const *v);
        llvm::Value* lower(IR::ASTValue const &v);

        IR::Unit const &unit;

        llvm::LLVMContext context;
        llvm::IRBuilder<> builder;
        llvm::Module mod;
        llvm::legacy::FunctionPassManager fpm;

        std::map<IR::Instrs::Instruction const *, llvm::Value*> values;
        std::map<IR::Block const *, llvm::BasicBlock*> blocks;
        std::map<IR::Function const *, llvm::Function*> functions;

        Mangle::NameMangler mangler;
    };
}
