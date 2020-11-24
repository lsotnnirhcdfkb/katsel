#include "ir/instruction.h"
#include "ir/visitor.h"
#include "lower/lowerer.h"

void Lower::Lowerer::visitStore(IR::Instrs::Store *instr)
{
    llvm::AllocaInst *target = allocas.at(instr->target);
    builder.CreateStore(lower(instr->value), target);
}

void Lower::Lowerer::visitIntCmpNE(IR::Instrs::IntCmpNE *instr) {} // TODO: signed and unsigned!!!
void Lower::Lowerer::visitIntCmpEQ(IR::Instrs::IntCmpEQ *instr) {}
void Lower::Lowerer::visitIntCmpULT(IR::Instrs::IntCmpULT *instr) {}
void Lower::Lowerer::visitIntCmpUGT(IR::Instrs::IntCmpUGT *instr) {}
void Lower::Lowerer::visitIntCmpULE(IR::Instrs::IntCmpULE *instr) {}
void Lower::Lowerer::visitIntCmpUGE(IR::Instrs::IntCmpUGE *instr) {}

#define BASICBINARYOP(instrname, llvmname)                                                                              \
    void Lower::Lowerer::visit##instrname(IR::Instrs::instrname *instr)                                                 \
    {                                                                                                                   \
        builder.CreateStore(builder.Create##llvmname(lower(instr->lhs), lower(instr->rhs)), allocas.at(instr->target)); \
    }
BASICBINARYOP(And, And)
BASICBINARYOP(Or, Or)

BASICBINARYOP(FloatCmpNE, FCmpONE)
BASICBINARYOP(FloatCmpEQ, FCmpOEQ)
BASICBINARYOP(FloatCmpLT, FCmpOLT)
BASICBINARYOP(FloatCmpGT, FCmpOGT)
BASICBINARYOP(FloatCmpLE, FCmpOLE)
BASICBINARYOP(FloatCmpGE, FCmpOGE)

BASICBINARYOP(BitXor, Xor)
BASICBINARYOP(BitOr, Or)
BASICBINARYOP(BitAnd, And)
BASICBINARYOP(ShiftR, LShr)
BASICBINARYOP(ShiftL, Shl)

void Lower::Lowerer::visitAdd(IR::Instrs::Add *instr) {} // TODO: fadd and add
void Lower::Lowerer::visitSub(IR::Instrs::Sub *instr) {} // TODO: sub and fsub
void Lower::Lowerer::visitMul(IR::Instrs::Mul *instr) {} // TODO: mul and fmul
void Lower::Lowerer::visitDiv(IR::Instrs::Div *instr) {} // TODO: udiv and sdiv and fdiv
void Lower::Lowerer::visitMod(IR::Instrs::Mod *instr) {} // TODO: urem and srem and frem

void Lower::Lowerer::visitNeg(IR::Instrs::Neg *instr) {} // TODO: fneg

void Lower::Lowerer::visitTrunc(IR::Instrs::Trunc *instr) {} // TODO: trunc, fptrunc

// TODO: these
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
