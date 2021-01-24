#include "ir/instruction.h"
#include "ir/visitor.h"
#include "ir/type.h"
#include "ir/function.h"
#include "lower/lowerer.h"
#include "utils/assert.h"

// Store and Phi instructions {{{1
void Lower::Lowerer::visit(IR::Instrs::Store &instr) {
    builder.CreateStore(lower(instr.value).as_raw(), lower(instr.target).as_raw());
}
void Lower::Lowerer::visit(IR::Instrs::Phi &instr) {
    NNPtr<llvm::PHINode> phi = llvm::PHINode::Create(instr.type()->to_llvmtype(context).as_raw(), instr.prevs.size());

    NNPtr<llvm::BasicBlock> current_block = builder.GetInsertBlock();

    for (auto &p : instr.prevs) {
        NNPtr<IR::Block> block = p.first;
        IR::ASTValue &value = p.second;

        builder.SetInsertPoint(blocks[block]);
        NNPtr<llvm::Value> valuellvm = lower(value);

        NNPtr<llvm::BasicBlock> blockllvm = blocks[block];

        phi->addIncoming(valuellvm.as_raw(), blockllvm.as_raw());
    }

    builder.SetInsertPoint(current_block.as_raw());
    builder.GetInsertBlock()->getInstList().push_back(phi.as_raw());

    values[instr] = phi.as_raw();
}
// Logical instructions {{{1
void Lower::Lowerer::visit(IR::Instrs::Or &instr) {
    values[instr] = builder.CreateOr(lower(instr.lhs).as_raw(), lower(instr.rhs).as_raw());
}
void Lower::Lowerer::visit(IR::Instrs::And &instr) {
    values[instr] = builder.CreateAnd(lower(instr.lhs).as_raw(), lower(instr.rhs).as_raw());
}
void Lower::Lowerer::visit(IR::Instrs::Not &instr) {
    values[instr] = builder.CreateICmpEQ(lower(instr.op).as_raw(), llvm::ConstantInt::get(llvm::Type::getInt1Ty(context), 0));
}
// Binary arithmetic instructions {{{1
#define DEF_FLOAT_BIN_INSTR(name, llvm_instr) \
    void Lower::Lowerer::visit(IR::Instrs::name &instr) { \
        values[instr] = builder.Create##llvm_instr(lower(instr.lhs).as_raw(), lower(instr.rhs).as_raw()); \
    }
#define DEF_INT_BIN_INSTR(name, if_signed_instr, if_unsigned_instr) \
    void Lower::Lowerer::visit(IR::Instrs::name &instr) { \
        NNPtr<IR::IntType> intty (static_cast<IR::IntType*>(instr.lhs.type().as_raw())); \
        if (intty->is_signed) \
            values[instr] = builder.Create##if_signed_instr(lower(instr.lhs).as_raw(), lower(instr.rhs).as_raw()); \
        else \
            values[instr] = builder.Create##if_unsigned_instr(lower(instr.lhs).as_raw(), lower(instr.rhs).as_raw()); \
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
void Lower::Lowerer::visit(IR::Instrs::FNeg &instr) {
    values[instr] = builder.CreateFNeg(lower(instr.op).as_raw());
}
void Lower::Lowerer::visit(IR::Instrs::INeg &instr) {
    values[instr] = builder.CreateSub(llvm::ConstantInt::get(instr.op.type()->to_llvmtype(context).as_raw(), 0), lower(instr.op).as_raw());
}
// Bitwise instructions {{{1
void Lower::Lowerer::visit(IR::Instrs::BitXor &instr) {
    values[instr] = builder.CreateXor(lower(instr.lhs).as_raw(), lower(instr.rhs).as_raw());
}
void Lower::Lowerer::visit(IR::Instrs::BitOr &instr) {
    values[instr] = builder.CreateOr(lower(instr.lhs).as_raw(), lower(instr.rhs).as_raw());
}
void Lower::Lowerer::visit(IR::Instrs::BitAnd &instr) {
    values[instr] = builder.CreateAnd(lower(instr.lhs).as_raw(), lower(instr.rhs).as_raw());
}
void Lower::Lowerer::visit(IR::Instrs::BitNot &instr) {
    values[instr] = builder.CreateXor(llvm::ConstantInt::get(instr.op.type()->to_llvmtype(context).as_raw(), -1), lower(instr.op).as_raw());
}
// Shift instructions {{{1
void Lower::Lowerer::visit(IR::Instrs::ShiftR &instr) {
    values[instr] = builder.CreateLShr(lower(instr.lhs).as_raw(), lower(instr.rhs).as_raw());
}
void Lower::Lowerer::visit(IR::Instrs::ShiftL &instr) {
    values[instr] = builder.CreateShl(lower(instr.lhs).as_raw(), lower(instr.rhs).as_raw());
}
// Type conversion instructions {{{1
void Lower::Lowerer::visit(IR::Instrs::NoOpCast &instr) {
    values[instr] = builder.CreateBitCast(lower(instr.op).as_raw(), instr.newt->to_llvmtype(context).as_raw());
}
void Lower::Lowerer::visit(IR::Instrs::FloatToFloat &instr) {
    NNPtr<IR::FloatType> bty = static_cast<IR::FloatType*>(instr.op.type().as_raw());

    if (bty->size < instr.newt->size)
        values[instr] = builder.CreateFPExt(lower(instr.op).as_raw(), instr.newt->to_llvmtype(context).as_raw());
    else if (bty->size > instr.newt->size)
        values[instr] = builder.CreateFPTrunc(lower(instr.op).as_raw(), instr.newt->to_llvmtype(context).as_raw());
    else
        values[instr] = lower(instr.op).as_raw(); // no cast needed
}
void Lower::Lowerer::visit(IR::Instrs::IntToInt &instr) {
    NNPtr<IR::IntType> bty = static_cast<IR::IntType*>(instr.op.type().as_raw());
    if (bty->size < instr.newt->size)
        if (bty->is_signed)
            values[instr] = builder.CreateSExt(lower(instr.op).as_raw(), instr.newt->to_llvmtype(context).as_raw());
        else
            values[instr] = builder.CreateZExt(lower(instr.op).as_raw(), instr.newt->to_llvmtype(context).as_raw());
    else if (bty->size > instr.newt->size)
        values[instr] = builder.CreateTrunc(lower(instr.op).as_raw(), instr.newt->to_llvmtype(context).as_raw());
    else
        values[instr] = lower(instr.op).as_raw();
}
void Lower::Lowerer::visit(IR::Instrs::IntToFloat &instr) {
    NNPtr<IR::IntType> bty = static_cast<IR::IntType*>(instr.op.type().as_raw());
    if (bty->is_signed)
        values[instr] = builder.CreateSIToFP(lower(instr.op).as_raw(), instr.newt->to_llvmtype(context).as_raw());
    else
        values[instr] = builder.CreateUIToFP(lower(instr.op).as_raw(), instr.newt->to_llvmtype(context).as_raw());
}
void Lower::Lowerer::visit(IR::Instrs::FloatToInt &instr) {
    if (instr.newt->is_signed)
        values[instr] = builder.CreateFPToSI(lower(instr.op).as_raw(), instr.newt->to_llvmtype(context).as_raw());
    else
        values[instr] = builder.CreateFPToUI(lower(instr.op).as_raw(), instr.newt->to_llvmtype(context).as_raw());
}
// Branch instructions {{{1
void Lower::Lowerer::visit(IR::Instrs::Call &instr) {
    std::vector<llvm::Value*> args;
    args.reserve(instr.args.size());
    for (IR::ASTValue &v : instr.args)
        args.push_back(lower(v).as_raw());

    llvm::Function *callee = static_cast<llvm::Function*>(lower(instr.f).as_raw());
    NNPtr<llvm::Value> res = builder.CreateCall(callee, args);

    if (!dynamic_cast<IR::VoidType*>(instr.type().as_raw()))
        values[instr] = res.as_raw();
}
// Pointer instruction {{{1
void Lower::Lowerer::visit(IR::Instrs::DerefPtr &instr) {
    values[instr] = builder.CreateLoad(lower(instr.ptr).as_raw());
}
void Lower::Lowerer::visit(IR::Instrs::Addrof &instr) {
    NNPtr<IR::Instrs::Instruction> addrof = dynamic_cast<IR::Instrs::Instruction*>(instr.deref->ptr.val.as_raw());
    values[instr] = values[addrof];
}
void Lower::Lowerer::visit(IR::Instrs::PtrArith &instr) {
    values[instr] = builder.CreateInBoundsGEP(lower(instr.ptr).as_raw(), { lower(instr.offset).as_raw() });
}
// Register instruction {{{1
void Lower::Lowerer::visit(IR::Instrs::Register &instr) {
    values[instr] = builder.CreateAlloca(instr.ty->to_llvmtype(context).as_raw());

    auto function_args = cur_function->args();
    if (alloca_index > 0 && alloca_index <= std::distance(function_args.begin(), function_args.end()))
        builder.CreateStore(cur_function->arg_begin() + (alloca_index - 1), values[instr]);

    ++alloca_index;
}
