#include "ir/instruction.h"
#include "ir/visitor.h"
#include "lower/lowerer.h"

void Lower::Lowerer::visitStore(IR::Instrs::Store *instr)
{
    llvm::AllocaInst *target = allocas.at(instr->target);
    builder.CreateStore(lower(instr->value), target);
}

void Lower::Lowerer::visitOr(IR::Instrs::Or *instr)
{

}
void Lower::Lowerer::visitAnd(IR::Instrs::And *instr)
{

}
void Lower::Lowerer::visitCmpNE(IR::Instrs::CmpNE *instr)
{

}
void Lower::Lowerer::visitCmpEQ(IR::Instrs::CmpEQ *instr)
{

}
void Lower::Lowerer::visitCmpLT(IR::Instrs::CmpLT *instr)
{

}
void Lower::Lowerer::visitCmpGT(IR::Instrs::CmpGT *instr)
{

}
void Lower::Lowerer::visitCmpLE(IR::Instrs::CmpLE *instr)
{

}
void Lower::Lowerer::visitCmpGE(IR::Instrs::CmpGE *instr)
{

}
void Lower::Lowerer::visitBitXor(IR::Instrs::BitXor *instr)
{

}
void Lower::Lowerer::visitBitOr(IR::Instrs::BitOr *instr)
{

}
void Lower::Lowerer::visitBitAnd(IR::Instrs::BitAnd *instr)
{

}
void Lower::Lowerer::visitBitNot(IR::Instrs::BitNot *instr)
{

}
void Lower::Lowerer::visitShiftR(IR::Instrs::ShiftR *instr)
{

}
void Lower::Lowerer::visitShiftL(IR::Instrs::ShiftL *instr)
{

}
void Lower::Lowerer::visitAdd(IR::Instrs::Add *instr)
{

}
void Lower::Lowerer::visitSub(IR::Instrs::Sub *instr)
{

}
void Lower::Lowerer::visitMult(IR::Instrs::Mult *instr)
{

}
void Lower::Lowerer::visitDiv(IR::Instrs::Div *instr)
{

}
void Lower::Lowerer::visitMod(IR::Instrs::Mod *instr)
{

}
void Lower::Lowerer::visitNeg(IR::Instrs::Neg *instr)
{

}
void Lower::Lowerer::visitTrunc(IR::Instrs::Trunc *instr)
{

}
void Lower::Lowerer::visitExt(IR::Instrs::Ext *instr)
{

}
void Lower::Lowerer::visitIntToFloat(IR::Instrs::IntToFloat *instr)
{

}
void Lower::Lowerer::visitFloatToInt(IR::Instrs::FloatToInt *instr)
{

}
void Lower::Lowerer::visitReturn(IR::Instrs::Return *instr)
{

}
void Lower::Lowerer::visitCall(IR::Instrs::Call *instr)
{

}
