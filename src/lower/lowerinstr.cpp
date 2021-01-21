#include "ir/instruction.h"
#include "ir/visitor.h"
#include "ir/type.h"
#include "lower/lowerer.h"
#include "utils/assert.h"

// Store and Phi instructions {{{1
void Lower::Lowerer::visitStore(IR::Instrs::Store &instr) {
    builder.CreateStore(lower(instr.value).asRaw(), lower(instr.target).asRaw());
}
void Lower::Lowerer::visitPhi(IR::Instrs::Phi &instr) {
    NNPtr<llvm::PHINode> phi = llvm::PHINode::Create(instr.type()->toLLVMType(context).asRaw(), instr.prevs.size());

    NNPtr<llvm::BasicBlock> currentBlock = builder.GetInsertBlock();

    for (auto &p : instr.prevs) {
        NNPtr<IR::Block> block = p.first;
        IR::ASTValue &value = p.second;

        builder.SetInsertPoint(blocks[block]);
        NNPtr<llvm::Value> valuellvm = lower(value);

        NNPtr<llvm::BasicBlock> blockllvm = blocks[block];

        phi->addIncoming(valuellvm.asRaw(), blockllvm.asRaw());
    }

    builder.SetInsertPoint(currentBlock.asRaw());
    builder.GetInsertBlock()->getInstList().push_back(phi.asRaw());

    values[instr] = phi.asRaw();
}
// Logical instructions {{{1
void Lower::Lowerer::visitOr(IR::Instrs::Or &instr) {
    values[instr] = builder.CreateOr(lower(instr.lhs).asRaw(), lower(instr.rhs).asRaw());
}
void Lower::Lowerer::visitAnd(IR::Instrs::And &instr) {
    values[instr] = builder.CreateAnd(lower(instr.lhs).asRaw(), lower(instr.rhs).asRaw());
}
void Lower::Lowerer::visitNot(IR::Instrs::Not &instr) {
    values[instr] = builder.CreateICmpEQ(lower(instr.op).asRaw(), llvm::ConstantInt::get(llvm::Type::getInt1Ty(context), 0));
}
// Binary arithmetic instructions {{{1
#define DEF_FLOAT_BIN_INSTR(name, llvmInstr) \
    void Lower::Lowerer::visit##name(IR::Instrs::name &instr) { \
        values[instr] = builder.Create##llvmInstr(lower(instr.lhs).asRaw(), lower(instr.rhs).asRaw()); \
    }
#define DEF_INT_BIN_INSTR(name, ifSignedInstr, ifUnsignedInstr) \
    void Lower::Lowerer::visit##name(IR::Instrs::name &instr) { \
        NNPtr<IR::IntType> intty (static_cast<IR::IntType*>(instr.lhs.type().asRaw())); \
        if (intty->isSigned) \
            values[instr] = builder.Create##ifSignedInstr(lower(instr.lhs).asRaw(), lower(instr.rhs).asRaw()); \
        else \
            values[instr] = builder.Create##ifUnsignedInstr(lower(instr.lhs).asRaw(), lower(instr.rhs).asRaw()); \
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
void Lower::Lowerer::visitFNeg(IR::Instrs::FNeg &instr) {
    values[instr] = builder.CreateFNeg(lower(instr.op).asRaw());
}
void Lower::Lowerer::visitINeg(IR::Instrs::INeg &instr) {
    values[instr] = builder.CreateSub(llvm::ConstantInt::get(instr.op.type()->toLLVMType(context).asRaw(), 0), lower(instr.op).asRaw());
}
// Bitwise instructions {{{1
void Lower::Lowerer::visitBitXor(IR::Instrs::BitXor &instr) {
    values[instr] = builder.CreateXor(lower(instr.lhs).asRaw(), lower(instr.rhs).asRaw());
}
void Lower::Lowerer::visitBitOr(IR::Instrs::BitOr &instr) {
    values[instr] = builder.CreateOr(lower(instr.lhs).asRaw(), lower(instr.rhs).asRaw());
}
void Lower::Lowerer::visitBitAnd(IR::Instrs::BitAnd &instr) {
    values[instr] = builder.CreateAnd(lower(instr.lhs).asRaw(), lower(instr.rhs).asRaw());
}
void Lower::Lowerer::visitBitNot(IR::Instrs::BitNot &instr) {
    values[instr] = builder.CreateXor(llvm::ConstantInt::get(instr.op.type()->toLLVMType(context).asRaw(), -1), lower(instr.op).asRaw());
}
// Shift instructions {{{1
void Lower::Lowerer::visitShiftR(IR::Instrs::ShiftR &instr) {
    values[instr] = builder.CreateLShr(lower(instr.lhs).asRaw(), lower(instr.rhs).asRaw());
}
void Lower::Lowerer::visitShiftL(IR::Instrs::ShiftL &instr) {
    values[instr] = builder.CreateShl(lower(instr.lhs).asRaw(), lower(instr.rhs).asRaw());
}
// Type conversion instructions {{{1
void Lower::Lowerer::visitNoOpCast(IR::Instrs::NoOpCast &instr) {
    values[instr] = builder.CreateBitCast(lower(instr.op).asRaw(), instr.newt->toLLVMType(context).asRaw());
}
void Lower::Lowerer::visitFloatToFloat(IR::Instrs::FloatToFloat &instr) {
    NNPtr<IR::FloatType> bty = static_cast<IR::FloatType*>(instr.op.type().asRaw());

    if (bty->size < instr.newt->size)
        values[instr] = builder.CreateFPExt(lower(instr.op).asRaw(), instr.newt->toLLVMType(context).asRaw());
    else if (bty->size > instr.newt->size)
        values[instr] = builder.CreateFPTrunc(lower(instr.op).asRaw(), instr.newt->toLLVMType(context).asRaw());
    else
        values[instr] = lower(instr.op).asRaw(); // no cast needed
}
void Lower::Lowerer::visitIntToInt(IR::Instrs::IntToInt &instr) {
    NNPtr<IR::IntType> bty = static_cast<IR::IntType*>(instr.op.type().asRaw());
    if (bty->size < instr.newt->size)
        if (bty->isSigned)
            values[instr] = builder.CreateSExt(lower(instr.op).asRaw(), instr.newt->toLLVMType(context).asRaw());
        else
            values[instr] = builder.CreateZExt(lower(instr.op).asRaw(), instr.newt->toLLVMType(context).asRaw());
    else if (bty->size > instr.newt->size)
        values[instr] = builder.CreateTrunc(lower(instr.op).asRaw(), instr.newt->toLLVMType(context).asRaw());
    else
        values[instr] = lower(instr.op).asRaw();
}
void Lower::Lowerer::visitIntToFloat(IR::Instrs::IntToFloat &instr) {
    NNPtr<IR::IntType> bty = static_cast<IR::IntType*>(instr.op.type().asRaw());
    if (bty->isSigned)
        values[instr] = builder.CreateSIToFP(lower(instr.op).asRaw(), instr.newt->toLLVMType(context).asRaw());
    else
        values[instr] = builder.CreateUIToFP(lower(instr.op).asRaw(), instr.newt->toLLVMType(context).asRaw());
}
void Lower::Lowerer::visitFloatToInt(IR::Instrs::FloatToInt &instr) {
    if (instr.newt->isSigned)
        values[instr] = builder.CreateFPToSI(lower(instr.op).asRaw(), instr.newt->toLLVMType(context).asRaw());
    else
        values[instr] = builder.CreateFPToUI(lower(instr.op).asRaw(), instr.newt->toLLVMType(context).asRaw());
}
// Branch instructions {{{1
void Lower::Lowerer::visitCall(IR::Instrs::Call &instr) {
    std::vector<llvm::Value*> args;
    args.reserve(instr.args.size());
    for (IR::ASTValue &v : instr.args)
        args.push_back(lower(v).asRaw());

    llvm::Function *callee = static_cast<llvm::Function*>(lower(instr.f).asRaw());
    NNPtr<llvm::Value> res = builder.CreateCall(callee, args);

    if (!dynamic_cast<IR::VoidType*>(instr.type().asRaw()))
        values[instr] = res.asRaw();
}
// Pointer instruction {{{1
void Lower::Lowerer::visitDerefPtr(IR::Instrs::DerefPtr &instr) {
    values[instr] = builder.CreateLoad(lower(instr.ptr).asRaw());
}
void Lower::Lowerer::visitAddrof(IR::Instrs::Addrof &instr) {
    NNPtr<IR::Instrs::Instruction> addrof = dynamic_cast<IR::Instrs::Instruction*>(instr.deref->ptr.val.asRaw());
    values[instr] = values[addrof];
}
void Lower::Lowerer::visitPtrArith(IR::Instrs::PtrArith &instr) {
    values[instr] = builder.CreateInBoundsGEP(lower(instr.ptr).asRaw(), { lower(instr.offset).asRaw() });
}
// Register instruction {{{1
void Lower::Lowerer::visitRegister(IR::Instrs::Register &instr) {
    values[instr] = builder.CreateAlloca(instr.ty->toLLVMType(context).asRaw());

    auto functionArgs = curFunction->args();
    if (allocaIndex > 0 && allocaIndex <= std::distance(functionArgs.begin(), functionArgs.end()))
        builder.CreateStore(curFunction->arg_begin() + (allocaIndex - 1), values[instr]);

    ++allocaIndex;
}
