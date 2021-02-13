#include "lower/lowerer.h"
#include "lowererlocal.h"

#include "ir/instruction.h"
#include "ir/visitor.h"
#include "ir/type.h"

#include "ir/function.h"

using namespace Lower;

#define SIGNATURE(instr_class) void LowerInstr::visit(IR::Instrs::instr_class const &instr)
#define CREATE_INSTR(instr) fl.builder.Create##instr
#define TARGET_REG() &fl.get_register(instr.target)
#define LOWER(v) &fl.value_ref.lower(v)
#define ASSIGN_TARGET(instr) CREATE_INSTR(Store)(TARGET_REG(), instr)

#define LOWER_LHS_RHS() LOWER(*instr.lhs.val), LOWER(*instr.rhs.val)

// Constructor {{{1
LowerInstr::LowerInstr(LowerFunction &fl): fl(fl) {}
// Copy {{{1
SIGNATURE(Copy) {
    ASSIGN_TARGET(LOWER(*instr.val.val));
}
// Logical instructions {{{1
SIGNATURE(Or) {
    ASSIGN_TARGET(CREATE_INSTR(Or)(LOWER_LHS_RHS()));
}
SIGNATURE(And) {
    ASSIGN_TARGET(CREATE_INSTR(And)(LOWER_LHS_RHS()));
}
SIGNATURE(Not) {
    ASSIGN_TARGET(CREATE_INSTR(ICmpEQ)(LOWER(*instr.op.val), llvm::ConstantInt::get(llvm::Type::getInt1Ty(fl.lowerer.context), 0)));
}
// Binary arithmetic instructions {{{1
#define DEF_FLOAT_BIN_INSTR(name, llvm_instr) \
    SIGNATURE(name) { \
        ASSIGN_TARGET(CREATE_INSTR(llvm_instr)(LOWER_LHS_RHS())); \
    }
#define DEF_INT_BIN_INSTR(name, if_signed_instr, if_unsigned_instr) \
    SIGNATURE(name) { \
        NNPtr<IR::IntType const> intty (static_cast<IR::IntType const *>(&instr.lhs.type())); \
        if (intty->is_signed) { \
            ASSIGN_TARGET(CREATE_INSTR(if_signed_instr)(LOWER_LHS_RHS())); \
        } else { \
            ASSIGN_TARGET(CREATE_INSTR(if_unsigned_instr)(LOWER_LHS_RHS())); \
        } \
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
SIGNATURE(FNeg) {
    ASSIGN_TARGET(CREATE_INSTR(FNeg)(LOWER(*instr.op.val)));
}
SIGNATURE(INeg) {
    ASSIGN_TARGET(CREATE_INSTR(Sub)(llvm::ConstantInt::get(&instr.op.type().to_llvmtype(fl.lowerer.context), 0), LOWER(*instr.op.val)));
}
// Bitwise instructions {{{1
SIGNATURE(BitXor) {
    ASSIGN_TARGET(CREATE_INSTR(Xor)(LOWER_LHS_RHS()));
}
SIGNATURE(BitOr) {
    ASSIGN_TARGET(CREATE_INSTR(Or)(LOWER_LHS_RHS()));
}
SIGNATURE(BitAnd) {
    ASSIGN_TARGET(CREATE_INSTR(And)(LOWER_LHS_RHS()));
}
SIGNATURE(BitNot) {
    ASSIGN_TARGET(CREATE_INSTR(Xor)(llvm::ConstantInt::get(&instr.op.type().to_llvmtype(fl.lowerer.context), -1), LOWER(*instr.op.val)));
}
// Shift instructions {{{1
SIGNATURE(ShiftR) {
    ASSIGN_TARGET(CREATE_INSTR(LShr)(LOWER_LHS_RHS()));
}
SIGNATURE(ShiftL) {
    ASSIGN_TARGET(CREATE_INSTR(Shl)(LOWER_LHS_RHS()));
}
// Type conversion instructions {{{1
SIGNATURE(NoOpCast) {
    ASSIGN_TARGET(CREATE_INSTR(BitCast)(LOWER(*instr.op.val), &instr.newt->to_llvmtype(fl.lowerer.context)));
}
SIGNATURE(FloatToFloat) {
    NNPtr<IR::FloatType const> bty = static_cast<IR::FloatType const *>(&instr.op.type());

    if (bty->size < instr.newt->size)
        ASSIGN_TARGET(CREATE_INSTR(FPExt)(LOWER(*instr.op.val), &instr.newt->to_llvmtype(fl.lowerer.context)));
    else if (bty->size > instr.newt->size)
        ASSIGN_TARGET(CREATE_INSTR(FPTrunc)(LOWER(*instr.op.val), &instr.newt->to_llvmtype(fl.lowerer.context)));
    else
        ASSIGN_TARGET(LOWER(*instr.op.val));
}
SIGNATURE(IntToInt) {
    NNPtr<IR::IntType const> bty = static_cast<IR::IntType const *>(&instr.op.type());
    if (bty->size < instr.newt->size)
        if (bty->is_signed)
            ASSIGN_TARGET(CREATE_INSTR(SExt)(LOWER(*instr.op.val), &instr.newt->to_llvmtype(fl.lowerer.context)));
        else
            ASSIGN_TARGET(CREATE_INSTR(ZExt)(LOWER(*instr.op.val), &instr.newt->to_llvmtype(fl.lowerer.context)));
    else if (bty->size > instr.newt->size)
        ASSIGN_TARGET(CREATE_INSTR(Trunc)(LOWER(*instr.op.val), &instr.newt->to_llvmtype(fl.lowerer.context)));
    else
        ASSIGN_TARGET(LOWER(*instr.op.val));
}
SIGNATURE(IntToFloat) {
    NNPtr<IR::IntType const> bty = static_cast<IR::IntType const *>(&instr.op.type());
    if (bty->is_signed)
        ASSIGN_TARGET(CREATE_INSTR(SIToFP)(LOWER(*instr.op.val), &instr.newt->to_llvmtype(fl.lowerer.context)));
    else
        ASSIGN_TARGET(CREATE_INSTR(UIToFP)(LOWER(*instr.op.val), &instr.newt->to_llvmtype(fl.lowerer.context)));
}
SIGNATURE(FloatToInt) {
    if (instr.newt->is_signed)
        ASSIGN_TARGET(CREATE_INSTR(FPToSI)(LOWER(*instr.op.val), &instr.newt->to_llvmtype(fl.lowerer.context)));
    else
        ASSIGN_TARGET(CREATE_INSTR(FPToUI)(LOWER(*instr.op.val), &instr.newt->to_llvmtype(fl.lowerer.context)));
}
// Branch instructions {{{1
SIGNATURE(Call) {
    std::vector<llvm::Value*> args;
    args.reserve(instr.args.size());
    for (IR::ASTValue const &v : instr.args)
        args.push_back(LOWER(*v.val));

    llvm::Function *callee = static_cast<llvm::Function*>(LOWER(*static_cast<IR::Value const *>(instr.f.as_raw())));
    ASSIGN_TARGET(CREATE_INSTR(Call)(callee, args));
}
// Pointer instruction {{{1
SIGNATURE(DerefPtr) {
    ASSIGN_TARGET(CREATE_INSTR(Load)(LOWER(*instr.ptr.val)));
}
SIGNATURE(Addrof) {
    ASSIGN_TARGET(&fl.get_register(instr.reg));
}
SIGNATURE(PtrArith) {
    ASSIGN_TARGET(CREATE_INSTR(InBoundsGEP)(LOWER(*instr.ptr.val), { LOWER(*instr.offset.val) }));
}
