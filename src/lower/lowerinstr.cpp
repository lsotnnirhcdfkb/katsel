#include "ir/instruction.h"
#include "ir/visitor.h"
#include "lower/lowerer.h"

void Lower::Lowerer::visitStore(IR::Instrs::Store *instr)
{
    llvm::AllocaInst *target = allocas.at(instr->target);
    builder.CreateStore(lower(instr->value), target);
}
void Lower::Lowerer::visitAnd(IR::Instrs::And *instr) {}
void Lower::Lowerer::visitOr(IR::Instrs::Or *instr) {}
void Lower::Lowerer::visitIntCmpNE(IR::Instrs::IntCmpNE *instr) {}
void Lower::Lowerer::visitIntCmpEQ(IR::Instrs::IntCmpEQ *instr) {}
void Lower::Lowerer::visitIntCmpULT(IR::Instrs::IntCmpULT *instr) {}
void Lower::Lowerer::visitIntCmpUGT(IR::Instrs::IntCmpUGT *instr) {}
void Lower::Lowerer::visitIntCmpULE(IR::Instrs::IntCmpULE *instr) {}
void Lower::Lowerer::visitIntCmpUGE(IR::Instrs::IntCmpUGE *instr) {}
void Lower::Lowerer::visitFloatCmpNE(IR::Instrs::FloatCmpNE *instr) {}
void Lower::Lowerer::visitFloatCmpEQ(IR::Instrs::FloatCmpEQ *instr) {}
void Lower::Lowerer::visitFloatCmpULT(IR::Instrs::FloatCmpULT *instr) {}
void Lower::Lowerer::visitFloatCmpUGT(IR::Instrs::FloatCmpUGT *instr) {}
void Lower::Lowerer::visitFloatCmpULE(IR::Instrs::FloatCmpULE *instr) {}
void Lower::Lowerer::visitFloatCmpUGE(IR::Instrs::FloatCmpUGE *instr) {}
void Lower::Lowerer::visitBitXor(IR::Instrs::BitXor *instr) {}
void Lower::Lowerer::visitBitOr(IR::Instrs::BitOr *instr) {}
void Lower::Lowerer::visitBitAnd(IR::Instrs::BitAnd *instr) {}
void Lower::Lowerer::visitBitNot(IR::Instrs::BitNot *instr) {}
void Lower::Lowerer::visitShiftR(IR::Instrs::ShiftR *instr) {}
void Lower::Lowerer::visitShiftL(IR::Instrs::ShiftL *instr) {}
void Lower::Lowerer::visitAdd(IR::Instrs::Add *instr) {}
void Lower::Lowerer::visitSub(IR::Instrs::Sub *instr) {}
void Lower::Lowerer::visitMult(IR::Instrs::Mult *instr) {}
void Lower::Lowerer::visitDiv(IR::Instrs::Div *instr) {}
void Lower::Lowerer::visitMod(IR::Instrs::Mod *instr) {}
void Lower::Lowerer::visitNeg(IR::Instrs::Neg *instr) {}
void Lower::Lowerer::visitTrunc(IR::Instrs::Trunc *instr) {}
void Lower::Lowerer::visitZeroExt(IR::Instrs::ZeroExt *instr) {}
void Lower::Lowerer::visitSignExt(IR::Instrs::SignExt *instr) {}
void Lower::Lowerer::visitFloatTrunc(IR::Instrs::FloatTrunc *instr) {}
void Lower::Lowerer::visitFloatExt(IR::Instrs::FloatExt *instr) {}
void Lower::Lowerer::visitSIntToFloat(IR::Instrs::SIntToFloat *instr) {}
void Lower::Lowerer::visitUIntToFloat(IR::Instrs::UIntToFloat *instr) {}
void Lower::Lowerer::visitFloatToSInt(IR::Instrs::FloatToSInt *instr) {}
void Lower::Lowerer::visitFloatToUInt(IR::Instrs::FloatToUInt *instr) {}
void Lower::Lowerer::visitReturn(IR::Instrs::Return *instr) {}
void Lower::Lowerer::visitCall(IR::Instrs::Call *instr) {}
