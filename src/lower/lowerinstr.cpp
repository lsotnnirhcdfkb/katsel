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
    builder.CreateStore(builder.CreateOr(lower(instr->lhs), lower(instr->rhs)), allocas.at(instr->target));
}
void Lower::Lowerer::visitAnd(IR::Instrs::And *instr)
{
    builder.CreateStore(builder.CreateAnd(lower(instr->lhs), lower(instr->rhs)), allocas.at(instr->target));
}
void Lower::Lowerer::visitCmpNE(IR::Instrs::CmpNE *instr)
{
    builder.CreateStore(builder.CreateICmpNE(lower(instr->lhs), lower(instr->rhs)), allocas.at(instr->target)); // TODO: unsigned and signed and fcmp
}
void Lower::Lowerer::visitCmpEQ(IR::Instrs::CmpEQ *instr)
{
    builder.CreateStore(builder.CreateICmpEQ(lower(instr->lhs), lower(instr->rhs)), allocas.at(instr->target)); // TODO: unsigned and signed and fcmp
}
void Lower::Lowerer::visitCmpLT(IR::Instrs::CmpLT *instr)
{
    builder.CreateStore(builder.CreateICmpULT(lower(instr->lhs), lower(instr->rhs)), allocas.at(instr->target));// TODO: unsigned and signed and fcmp
}
void Lower::Lowerer::visitCmpGT(IR::Instrs::CmpGT *instr)
{
    builder.CreateStore(builder.CreateICmpUGT(lower(instr->lhs), lower(instr->rhs)), allocas.at(instr->target)); // TODO: unsigned and signed and fcmp
}
void Lower::Lowerer::visitCmpLE(IR::Instrs::CmpLE *instr)
{
    builder.CreateStore(builder.CreateICmpULE(lower(instr->lhs), lower(instr->rhs)), allocas.at(instr->target)); // TODO: unsigned and signed and fcmp
}
void Lower::Lowerer::visitCmpGE(IR::Instrs::CmpGE *instr)
{
    builder.CreateStore(builder.CreateICmpUGE(lower(instr->lhs), lower(instr->rhs)), allocas.at(instr->target)); // TODO: unsigned and signed and fcmp
}
void Lower::Lowerer::visitBitXor(IR::Instrs::BitXor *instr)
{
    builder.CreateStore(builder.CreateXor(lower(instr->lhs), lower(instr->rhs)), allocas.at(instr->target));
}
void Lower::Lowerer::visitBitOr(IR::Instrs::BitOr *instr)
{
    builder.CreateStore(builder.CreateOr(lower(instr->lhs), lower(instr->rhs)), allocas.at(instr->target));
}
void Lower::Lowerer::visitBitAnd(IR::Instrs::BitAnd *instr)
{
    builder.CreateStore(builder.CreateAnd(lower(instr->lhs), lower(instr->rhs)), allocas.at(instr->target));
}
void Lower::Lowerer::visitBitNot(IR::Instrs::BitNot *instr)
{
    builder.CreateStore(builder.CreateXor(llvm::ConstantInt::get(instr->op->type()->toLLVMType(context), -1), lower(instr->op)), allocas.at(instr->target));
}
void Lower::Lowerer::visitShiftR(IR::Instrs::ShiftR *instr)
{
    builder.CreateStore(builder.CreateLShr(lower(instr->lhs), lower(instr->rhs)), allocas.at(instr->target));
}
void Lower::Lowerer::visitShiftL(IR::Instrs::ShiftL *instr)
{
    builder.CreateStore(builder.CreateShl(lower(instr->lhs), lower(instr->rhs)), allocas.at(instr->target));
}
void Lower::Lowerer::visitAdd(IR::Instrs::Add *instr)
{
    builder.CreateStore(builder.CreateAdd(lower(instr->lhs), lower(instr->rhs)), allocas.at(instr->target)); // TODO: fadd
}
void Lower::Lowerer::visitSub(IR::Instrs::Sub *instr)
{
    builder.CreateStore(builder.CreateSub(lower(instr->lhs), lower(instr->rhs)), allocas.at(instr->target)); // TODO: fsub
}
void Lower::Lowerer::visitMult(IR::Instrs::Mult *instr)
{
    builder.CreateStore(builder.CreateMul(lower(instr->lhs), lower(instr->rhs)), allocas.at(instr->target)); // TODO: fmul
}
void Lower::Lowerer::visitDiv(IR::Instrs::Div *instr)
{
    builder.CreateStore(builder.CreateUDiv(lower(instr->lhs), lower(instr->rhs)), allocas.at(instr->target)); // TODO: udiv, sdiv, and fdiv
}
void Lower::Lowerer::visitMod(IR::Instrs::Mod *instr)
{
    builder.CreateStore(builder.CreateURem(lower(instr->lhs), lower(instr->rhs)), allocas.at(instr->target)); // TODO: urem, srem, and frem
}
void Lower::Lowerer::visitNeg(IR::Instrs::Neg *instr)
{
    builder.CreateStore(builder.CreateSub(llvm::ConstantInt::get(instr->op->type()->toLLVMType(context), 0), lower(instr->op)), allocas.at(instr->target)); // TODO: fneg
}
void Lower::Lowerer::visitTrunc(IR::Instrs::Trunc *instr)
{
    builder.CreateStore(builder.CreateTrunc(lower(instr->op), instr->newt->toLLVMType(context)), allocas.at(instr->target)); // TODO: fptrunc and trunc
}
void Lower::Lowerer::visitExt(IR::Instrs::Ext *instr)
{
    builder.CreateStore(builder.CreateZExt(lower(instr->op), instr->newt->toLLVMType(context)), allocas.at(instr->target)); // TODO: signed and unsigned and fpext
}
void Lower::Lowerer::visitIntToFloat(IR::Instrs::IntToFloat *instr)
{
    builder.CreateStore(builder.CreateUIToFP(lower(instr->op), instr->newt->toLLVMType(context)), allocas.at(instr->target)); // TODO: SIToFP, UIToFP (signed and unsigned)
}
void Lower::Lowerer::visitFloatToInt(IR::Instrs::FloatToInt *instr)
{
    builder.CreateStore(builder.CreateFPToUI(lower(instr->op), instr->newt->toLLVMType(context)), allocas.at(instr->target)); // TODO: FPTOSI, FPToUI (signed and unsigned)
}
void Lower::Lowerer::visitReturn(IR::Instrs::Return *instr)
{
    if (instr->value)
        builder.CreateRet(lower(instr->value));
    else
        builder.CreateRetVoid();
}
void Lower::Lowerer::visitCall(IR::Instrs::Call *instr)
{
    std::vector<llvm::Value*> args;
    args.reserve(instr->args.size());
    for (IR::Value const *v : instr->args)
        args.push_back(lower(v));

    llvm::Function *callee = static_cast<llvm::Function*>(lower(instr->f));
    llvm::Value *res = builder.CreateCall(callee, args);

    if (instr->reg)
        builder.CreateStore(res, allocas.at(instr->reg));
}
