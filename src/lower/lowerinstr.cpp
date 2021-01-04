#include "ir/instruction.h"
#include "ir/visitor.h"
#include "ir/type.h"
#include "lower/lowerer.h"
#include "utils/assert.h"

// Store and Phi instructions {{{1
void Lower::Lowerer::visitStore(IR::Instrs::Store *instr) {
    if (dynamic_cast<IR::VoidType*>(static_cast<IR::PointerType*>(instr->target.type())->ty))
        return;

    builder.CreateStore(lower(instr->value), lower(instr->target));
}
void Lower::Lowerer::visitPhi(IR::Instrs::Phi *instr) {
    llvm::PHINode *phi = llvm::PHINode::Create(instr->type()->toLLVMType(context), instr->prevs.size());

    llvm::BasicBlock *currentBlock = builder.GetInsertBlock();

    for (auto &p : instr->prevs) {
        IR::Block *block = p.first;
        IR::ASTValue &value = p.second;

        builder.SetInsertPoint(blocks[block]);
        llvm::Value *valuellvm = lower(value);

        llvm::BasicBlock *blockllvm = blocks[block];

        phi->addIncoming(valuellvm, blockllvm);
    }

    builder.SetInsertPoint(currentBlock);
    builder.GetInsertBlock()->getInstList().push_back(phi);

    values[instr] = phi;
}
// Logical instructions {{{1
void Lower::Lowerer::visitOr(IR::Instrs::Or *instr) {
    values[instr] = builder.CreateOr(lower(instr->lhs), lower(instr->rhs));
}
void Lower::Lowerer::visitAnd(IR::Instrs::And *instr) {
    values[instr] = builder.CreateAnd(lower(instr->lhs), lower(instr->rhs));
}
void Lower::Lowerer::visitNot(IR::Instrs::Not *instr) {
    values[instr] = builder.CreateICmpEQ(lower(instr->op), llvm::ConstantInt::get(llvm::Type::getInt1Ty(context), 0));
}
// Binary arithmetic instructions {{{1
#define DEF_FLOAT_BIN_INSTR(name, llvmInstr) \
    void Lower::Lowerer::visit##name(IR::Instrs::name *instr) {                          \
        values[instr] = builder.Create##llvmInstr(lower(instr->lhs), lower(instr->rhs)); \
    }
#define DEF_INT_BIN_INSTR(name, ifSignedInstr, ifUnsignedInstr) \
    void Lower::Lowerer::visit##name(IR::Instrs::name *instr) {                                    \
        IR::IntType *intty (static_cast<IR::IntType*>(instr->lhs.type()));                         \
        if (intty->isSigned)                                                                       \
            values[instr] = builder.Create##ifSignedInstr(lower(instr->lhs), lower(instr->rhs));   \
        else                                                                                       \
            values[instr] = builder.Create##ifUnsignedInstr(lower(instr->lhs), lower(instr->rhs)); \
    }

DEF_FLOAT_BIN_INSTR(FCmpNE, FCmpONE)
DEF_FLOAT_BIN_INSTR(FCmpEQ, FCmpOEQ)
DEF_FLOAT_BIN_INSTR(FCmpLT, FCmpOLT)
DEF_FLOAT_BIN_INSTR(FCmpGT, FCmpOGT)
DEF_FLOAT_BIN_INSTR(FCmpLE, FCmpOLE)
DEF_FLOAT_BIN_INSTR(FCmpGE, FCmpOGE)
DEF_FLOAT_BIN_INSTR(FAdd  , FAdd)
DEF_FLOAT_BIN_INSTR(FSub  , FSub)
DEF_FLOAT_BIN_INSTR(FMult , FMul)
DEF_FLOAT_BIN_INSTR(FDiv  , FDiv)
DEF_FLOAT_BIN_INSTR(FMod  , FRem)

DEF_INT_BIN_INSTR(ICmpNE, ICmpNE , ICmpNE )
DEF_INT_BIN_INSTR(ICmpEQ, ICmpEQ , ICmpEQ )
DEF_INT_BIN_INSTR(ICmpLT, ICmpSLT, ICmpULT)
DEF_INT_BIN_INSTR(ICmpGT, ICmpSGT, ICmpUGT)
DEF_INT_BIN_INSTR(ICmpLE, ICmpSLE, ICmpULE)
DEF_INT_BIN_INSTR(ICmpGE, ICmpSGE, ICmpUGE)
DEF_INT_BIN_INSTR(IAdd  , Add    , Add    )
DEF_INT_BIN_INSTR(ISub  , Sub    , Sub    )
DEF_INT_BIN_INSTR(IMult , Mul    , Mul    )
DEF_INT_BIN_INSTR(IDiv  , SDiv   , UDiv   )
DEF_INT_BIN_INSTR(IMod  , SRem   , URem   )
#undef DEF_FLOAT_BIN_INSTR
#undef DEF_INT_BIN_INSTR
// Unary arithmetic instructions {{{1
void Lower::Lowerer::visitFNeg(IR::Instrs::FNeg *instr) {
    values[instr] = builder.CreateFNeg(lower(instr->op));
}
void Lower::Lowerer::visitINeg(IR::Instrs::INeg *instr) {
    values[instr] = builder.CreateSub(llvm::ConstantInt::get(instr->op.type()->toLLVMType(context), 0), lower(instr->op));
}
// Bitwise instructions {{{1
void Lower::Lowerer::visitBitXor(IR::Instrs::BitXor *instr) {
    values[instr] = builder.CreateXor(lower(instr->lhs), lower(instr->rhs));
}
void Lower::Lowerer::visitBitOr(IR::Instrs::BitOr *instr) {
    values[instr] = builder.CreateOr(lower(instr->lhs), lower(instr->rhs));
}
void Lower::Lowerer::visitBitAnd(IR::Instrs::BitAnd *instr) {
    values[instr] = builder.CreateAnd(lower(instr->lhs), lower(instr->rhs));
}
void Lower::Lowerer::visitBitNot(IR::Instrs::BitNot *instr) {
    values[instr] = builder.CreateXor(llvm::ConstantInt::get(instr->op.type()->toLLVMType(context), -1), lower(instr->op));
}
// Shift instructions {{{1
void Lower::Lowerer::visitShiftR(IR::Instrs::ShiftR *instr) {
    values[instr] = builder.CreateLShr(lower(instr->lhs), lower(instr->rhs));
}
void Lower::Lowerer::visitShiftL(IR::Instrs::ShiftL *instr) {
    values[instr] = builder.CreateShl(lower(instr->lhs), lower(instr->rhs));
}
// Type conversion instructions {{{1
void Lower::Lowerer::visitNoOpCast(IR::Instrs::NoOpCast *instr) {
    values[instr] = builder.CreateBitCast(lower(instr->op), instr->newt->toLLVMType(context));
}
void Lower::Lowerer::visitFloatToFloat(IR::Instrs::FloatToFloat *instr) {
    IR::FloatType *bty = static_cast<IR::FloatType*>(instr->op.type());

    if (bty->size < instr->newt->size)
        values[instr] = builder.CreateFPExt(lower(instr->op), instr->newt->toLLVMType(context));
    else if (bty->size > instr->newt->size)
        values[instr] = builder.CreateFPTrunc(lower(instr->op), instr->newt->toLLVMType(context));
    else
        values[instr] = lower(instr->op); // no cast needed
}
void Lower::Lowerer::visitIntToInt(IR::Instrs::IntToInt *instr) {
    IR::IntType *bty = static_cast<IR::IntType*>(instr->op.type());
    if (bty->size < instr->newt->size)
        if (bty->isSigned)
            values[instr] = builder.CreateSExt(lower(instr->op), instr->newt->toLLVMType(context));
        else
            values[instr] = builder.CreateZExt(lower(instr->op), instr->newt->toLLVMType(context));
    else if (bty->size > instr->newt->size)
        values[instr] = builder.CreateTrunc(lower(instr->op), instr->newt->toLLVMType(context));
    else
        values[instr] = lower(instr->op);
}
void Lower::Lowerer::visitIntToFloat(IR::Instrs::IntToFloat *instr) {
    IR::IntType *bty = static_cast<IR::IntType*>(instr->op.type());
    if (bty->isSigned)
        values[instr] = builder.CreateSIToFP(lower(instr->op), instr->newt->toLLVMType(context));
    else
        values[instr] = builder.CreateUIToFP(lower(instr->op), instr->newt->toLLVMType(context));
}
void Lower::Lowerer::visitFloatToInt(IR::Instrs::FloatToInt *instr) {
    if (instr->newt->isSigned)
        values[instr] = builder.CreateFPToSI(lower(instr->op), instr->newt->toLLVMType(context));
    else
        values[instr] = builder.CreateFPToUI(lower(instr->op), instr->newt->toLLVMType(context));
}
// Branch instructions {{{1
void Lower::Lowerer::visitCall(IR::Instrs::Call *instr) {
    std::vector<llvm::Value*> args;
    args.reserve(instr->args.size());
    for (IR::ASTValue &v : instr->args)
        args.push_back(lower(v));

    llvm::Function *callee = static_cast<llvm::Function*>(lower(instr->f));
    llvm::Value *res = builder.CreateCall(callee, args);

    if (!dynamic_cast<IR::VoidType*>(instr->type()))
        values[instr] = res;
}
// Pointer instruction {{{1
void Lower::Lowerer::visitDerefPtr(IR::Instrs::DerefPtr *instr) {
    values[instr] = builder.CreateLoad(lower(instr->ptr));
}
void Lower::Lowerer::visitPtrArith(IR::Instrs::PtrArith *instr) {
    values[instr] = builder.CreateInBoundsGEP(lower(instr->ptr), { lower(instr->offset) });
}
// Register instruction {{{1
void Lower::Lowerer::visitRegister(IR::Instrs::Register *instr) {
    values[instr] = builder.CreateAlloca(instr->ty->toLLVMType(context));

    auto functionArgs = curFunction->args();
    if (allocaIndex > 0 && allocaIndex <= std::distance(functionArgs.begin(), functionArgs.end()))
        builder.CreateStore(curFunction->arg_begin() + (allocaIndex - 1), values[instr]);

    ++allocaIndex;
}
