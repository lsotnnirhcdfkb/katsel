#include "ir/instruction.h"
#include "ir/visitor.h"
#include "ir/type.h"
#include "ir/function.h"
#include "lower/lowerer.h"
#include "utils/assert.h"

// Store and Phi instructions {{{1
void Lower::Lowerer::visit(IR::Instrs::Store const &instr) {
    builder.CreateStore(&lower(instr.value), &lower(instr.target));
}
void Lower::Lowerer::visit(IR::Instrs::Phi const &instr) {
    NNPtr<llvm::PHINode> phi = llvm::PHINode::Create(&instr.type().to_llvmtype(context), instr.prevs.size());

    NNPtr<llvm::BasicBlock> current_block = builder.GetInsertBlock();

    for (auto &p : instr.prevs) {
        NNPtr<IR::Block const> block = p.first;
        IR::ASTValue const &value = p.second;

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
void Lower::Lowerer::visit(IR::Instrs::Or const &instr) {
    values[instr] = builder.CreateOr(&lower(instr.lhs), &lower(instr.rhs));
}
void Lower::Lowerer::visit(IR::Instrs::And const &instr) {
    values[instr] = builder.CreateAnd(&lower(instr.lhs), &lower(instr.rhs));
}
void Lower::Lowerer::visit(IR::Instrs::Not const &instr) {
    values[instr] = builder.CreateICmpEQ(&lower(instr.op), llvm::ConstantInt::get(llvm::Type::getInt1Ty(context), 0));
}
// Binary arithmetic instructions {{{1
#define DEF_FLOAT_BIN_INSTR(name, llvm_instr) \
    void Lower::Lowerer::visit(IR::Instrs::name const &instr) { \
        values[instr] = builder.Create##llvm_instr(&lower(instr.lhs), &lower(instr.rhs)); \
    }
#define DEF_INT_BIN_INSTR(name, if_signed_instr, if_unsigned_instr) \
    void Lower::Lowerer::visit(IR::Instrs::name const &instr) { \
        NNPtr<IR::IntType const> intty (static_cast<IR::IntType const *>(&instr.lhs.type())); \
        if (intty->is_signed) \
            values[instr] = builder.Create##if_signed_instr(&lower(instr.lhs), &lower(instr.rhs)); \
        else \
            values[instr] = builder.Create##if_unsigned_instr(&lower(instr.lhs), &lower(instr.rhs)); \
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
void Lower::Lowerer::visit(IR::Instrs::FNeg const &instr) {
    values[instr] = builder.CreateFNeg(&lower(instr.op));
}
void Lower::Lowerer::visit(IR::Instrs::INeg const &instr) {
    values[instr] = builder.CreateSub(llvm::ConstantInt::get(&instr.op.type().to_llvmtype(context), 0), &lower(instr.op));
}
// Bitwise instructions {{{1
void Lower::Lowerer::visit(IR::Instrs::BitXor const &instr) {
    values[instr] = builder.CreateXor(&lower(instr.lhs), &lower(instr.rhs));
}
void Lower::Lowerer::visit(IR::Instrs::BitOr const &instr) {
    values[instr] = builder.CreateOr(&lower(instr.lhs), &lower(instr.rhs));
}
void Lower::Lowerer::visit(IR::Instrs::BitAnd const &instr) {
    values[instr] = builder.CreateAnd(&lower(instr.lhs), &lower(instr.rhs));
}
void Lower::Lowerer::visit(IR::Instrs::BitNot const &instr) {
    values[instr] = builder.CreateXor(llvm::ConstantInt::get(&instr.op.type().to_llvmtype(context), -1), &lower(instr.op));
}
// Shift instructions {{{1
void Lower::Lowerer::visit(IR::Instrs::ShiftR const &instr) {
    values[instr] = builder.CreateLShr(&lower(instr.lhs), &lower(instr.rhs));
}
void Lower::Lowerer::visit(IR::Instrs::ShiftL const &instr) {
    values[instr] = builder.CreateShl(&lower(instr.lhs), &lower(instr.rhs));
}
// Type conversion instructions {{{1
void Lower::Lowerer::visit(IR::Instrs::NoOpCast const &instr) {
    values[instr] = builder.CreateBitCast(&lower(instr.op), &instr.newt->to_llvmtype(context));
}
void Lower::Lowerer::visit(IR::Instrs::FloatToFloat const &instr) {
    NNPtr<IR::FloatType const> bty = static_cast<IR::FloatType const *>(&instr.op.type());

    if (bty->size < instr.newt->size)
        values[instr] = builder.CreateFPExt(&lower(instr.op), &instr.newt->to_llvmtype(context));
    else if (bty->size > instr.newt->size)
        values[instr] = builder.CreateFPTrunc(&lower(instr.op), &instr.newt->to_llvmtype(context));
    else
        values[instr] = &lower(instr.op); // no cast needed
}
void Lower::Lowerer::visit(IR::Instrs::IntToInt const &instr) {
    NNPtr<IR::IntType const> bty = static_cast<IR::IntType const *>(&instr.op.type());
    if (bty->size < instr.newt->size)
        if (bty->is_signed)
            values[instr] = builder.CreateSExt(&lower(instr.op), &instr.newt->to_llvmtype(context));
        else
            values[instr] = builder.CreateZExt(&lower(instr.op), &instr.newt->to_llvmtype(context));
    else if (bty->size > instr.newt->size)
        values[instr] = builder.CreateTrunc(&lower(instr.op), &instr.newt->to_llvmtype(context));
    else
        values[instr] = &lower(instr.op);
}
void Lower::Lowerer::visit(IR::Instrs::IntToFloat const &instr) {
    NNPtr<IR::IntType const> bty = static_cast<IR::IntType const *>(&instr.op.type());
    if (bty->is_signed)
        values[instr] = builder.CreateSIToFP(&lower(instr.op), &instr.newt->to_llvmtype(context));
    else
        values[instr] = builder.CreateUIToFP(&lower(instr.op), &instr.newt->to_llvmtype(context));
}
void Lower::Lowerer::visit(IR::Instrs::FloatToInt const &instr) {
    if (instr.newt->is_signed)
        values[instr] = builder.CreateFPToSI(&lower(instr.op), &instr.newt->to_llvmtype(context));
    else
        values[instr] = builder.CreateFPToUI(&lower(instr.op), &instr.newt->to_llvmtype(context));
}
// Branch instructions {{{1
void Lower::Lowerer::visit(IR::Instrs::Call const &instr) {
    std::vector<llvm::Value*> args;
    args.reserve(instr.args.size());
    for (IR::ASTValue const &v : instr.args)
        args.push_back(&lower(v));

    llvm::Function *callee = static_cast<llvm::Function*>(&lower(*static_cast<IR::Value const *>(instr.f.as_raw())));
    NNPtr<llvm::Value> res = builder.CreateCall(callee, args);

    if (!dynamic_cast<IR::VoidType const *>(&instr.type()))
        values[instr] = res.as_raw();
}
// Pointer instruction {{{1
void Lower::Lowerer::visit(IR::Instrs::DerefPtr const &instr) {
    values[instr] = builder.CreateLoad(&lower(instr.ptr));
}
void Lower::Lowerer::visit(IR::Instrs::Addrof const &instr) {
    NNPtr<IR::Instrs::Instruction const> addrof = dynamic_cast<IR::Instrs::Instruction const *>(instr.deref->ptr.val.as_raw());
    values[instr] = values[addrof];
}
void Lower::Lowerer::visit(IR::Instrs::PtrArith const &instr) {
    values[instr] = builder.CreateInBoundsGEP(&lower(instr.ptr), { &lower(instr.offset) });
}
// Register instruction {{{1
void Lower::Lowerer::visit(IR::Instrs::Register const &instr) {
    values[instr] = builder.CreateAlloca(&instr.ty->to_llvmtype(context));

    auto function_args = cur_function->args();
    if (alloca_index > 0 && alloca_index <= std::distance(function_args.begin(), function_args.end()))
        builder.CreateStore(cur_function->arg_begin() + (alloca_index - 1), values[instr]);

    ++alloca_index;
}
