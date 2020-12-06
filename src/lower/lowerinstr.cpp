#include "ir/instruction.h"
#include "ir/visitor.h"
#include "ir/type.h"
#include "lower/lowerer.h"

void Lower::Lowerer::visitStore(IR::Instrs::Store *instr)
{
    llvm::AllocaInst *target = allocas.at(instr->target);
    builder.CreateStore(lower(instr->value), target);
}
void Lower::Lowerer::visitPhi(IR::Instrs::Phi *instr)
{
    llvm::PHINode *phi = builder.CreatePHI(instr->target->type()->toLLVMType(context), instr->prevs.size());
    for (auto &p : instr->prevs)
        phi->addIncoming(lower(p.second), blocks[p.first]);
    tempregisters[instr->target] = phi;
}

void Lower::Lowerer::visitOr(IR::Instrs::Or *instr)
{
    tempregisters[instr->target] = builder.CreateOr(lower(instr->lhs), lower(instr->rhs));
}
void Lower::Lowerer::visitAnd(IR::Instrs::And *instr)
{
    tempregisters[instr->target] = builder.CreateAnd(lower(instr->lhs), lower(instr->rhs));
}

#define DEF_BIN_INSTR(name, ifFloating, ifSignedInt, ifUnsignedInt)                                                               \
    void Lower::Lowerer::visit##name(IR::Instrs::name *instr)                                                                     \
    {                                                                                                                             \
        IR::BuiltinType *bty = static_cast<IR::BuiltinType*>(instr->lhs.type());                                                  \
        if (bty->isFloating())                                                                                                    \
            tempregisters[instr->target] = builder.Create##ifFloating (lower(instr->lhs), lower(instr->rhs));                     \
        else if (bty->isSigned())                                                                                                 \
            tempregisters[instr->target] = builder.Create##ifSignedInt (lower(instr->lhs), lower(instr->rhs));                    \
        else                                                                                                                      \
            tempregisters[instr->target] = builder.Create##ifUnsignedInt (lower(instr->lhs), lower(instr->rhs));                  \
    }

//            name     ifFloating    ifSignedInt    ifUnsignedInt
DEF_BIN_INSTR(CmpNE  , FCmpONE     , ICmpNE       , ICmpNE      )
DEF_BIN_INSTR(CmpEQ  , FCmpOEQ     , ICmpEQ       , ICmpEQ      )
DEF_BIN_INSTR(CmpLT  , FCmpOLT     , ICmpSLT      , ICmpULT     )
DEF_BIN_INSTR(CmpGT  , FCmpOGT     , ICmpSGT      , ICmpUGT     )
DEF_BIN_INSTR(CmpLE  , FCmpOLE     , ICmpSLE      , ICmpULE     )
DEF_BIN_INSTR(CmpGE  , FCmpOGE     , ICmpSGE      , ICmpUGE     )

DEF_BIN_INSTR(Add    , FAdd        , Add          , Add         )
DEF_BIN_INSTR(Sub    , FSub        , Sub          , Sub         )
DEF_BIN_INSTR(Mult   , FMul        , Mul          , Mul         )
DEF_BIN_INSTR(Div    , FDiv        , SDiv         , UDiv        )
DEF_BIN_INSTR(Mod    , FRem        , SRem         , URem        )
#undef DEF_BIN_INSTR

void Lower::Lowerer::visitBitXor(IR::Instrs::BitXor *instr)
{
    tempregisters[instr->target] = builder.CreateXor(lower(instr->lhs), lower(instr->rhs));
}
void Lower::Lowerer::visitBitOr(IR::Instrs::BitOr *instr)
{
    tempregisters[instr->target] = builder.CreateOr(lower(instr->lhs), lower(instr->rhs));
}
void Lower::Lowerer::visitBitAnd(IR::Instrs::BitAnd *instr)
{
    tempregisters[instr->target] = builder.CreateAnd(lower(instr->lhs), lower(instr->rhs));
}
void Lower::Lowerer::visitBitNot(IR::Instrs::BitNot *instr)
{
    tempregisters[instr->target] = builder.CreateXor(llvm::ConstantInt::get(instr->op.type()->toLLVMType(context), -1), lower(instr->op));
}
void Lower::Lowerer::visitShiftR(IR::Instrs::ShiftR *instr)
{
    tempregisters[instr->target] = builder.CreateLShr(lower(instr->lhs), lower(instr->rhs));
}
void Lower::Lowerer::visitShiftL(IR::Instrs::ShiftL *instr)
{
    tempregisters[instr->target] = builder.CreateShl(lower(instr->lhs), lower(instr->rhs));
}
void Lower::Lowerer::visitNeg(IR::Instrs::Neg *instr)
{
    IR::BuiltinType *bty = static_cast<IR::BuiltinType*>(instr->op.type());
    if (bty->isFloating())
        tempregisters[instr->target] = builder.CreateFNeg(lower(instr->op));
    else
        tempregisters[instr->target] = builder.CreateSub(llvm::ConstantInt::get(instr->op.type()->toLLVMType(context), 0), lower(instr->op));
}
void Lower::Lowerer::visitTrunc(IR::Instrs::Trunc *instr)
{
    IR::BuiltinType *bty = static_cast<IR::BuiltinType*>(instr->op.type());
    if (bty->isFloating())
        tempregisters[instr->target] = builder.CreateFPTrunc(lower(instr->op), instr->newt->toLLVMType(context));
    else
        tempregisters[instr->target] = builder.CreateTrunc(lower(instr->op), instr->newt->toLLVMType(context));
}
void Lower::Lowerer::visitExt(IR::Instrs::Ext *instr)
{
    IR::BuiltinType *bty = static_cast<IR::BuiltinType*>(instr->op.type());
    if (bty->isFloating())
        tempregisters[instr->target] = builder.CreateFPExt(lower(instr->op), instr->newt->toLLVMType(context));
    else if (bty->isSigned())
        tempregisters[instr->target] = builder.CreateSExt(lower(instr->op), instr->newt->toLLVMType(context));
    else
        tempregisters[instr->target] = builder.CreateZExt(lower(instr->op), instr->newt->toLLVMType(context));
}
void Lower::Lowerer::visitIntToFloat(IR::Instrs::IntToFloat *instr)
{
    IR::BuiltinType *bty = static_cast<IR::BuiltinType*>(instr->op.type());
    if (bty->isSigned())
        tempregisters[instr->target] = builder.CreateSIToFP(lower(instr->op), instr->newt->toLLVMType(context));
    else
        tempregisters[instr->target] = builder.CreateUIToFP(lower(instr->op), instr->newt->toLLVMType(context));
}
void Lower::Lowerer::visitFloatToInt(IR::Instrs::FloatToInt *instr)
{
    if (instr->newt->isSigned())
        tempregisters[instr->target] = builder.CreateFPToSI(lower(instr->op), instr->newt->toLLVMType(context));
    else
        tempregisters[instr->target] = builder.CreateFPToUI(lower(instr->op), instr->newt->toLLVMType(context));
}
void Lower::Lowerer::visitReturn(IR::Instrs::Return *instr)
{
    if (!dynamic_cast<IR::VoidType*>(instr->value->type()))
        builder.CreateRet(lower(instr->value));
    else
        builder.CreateRetVoid();
}
void Lower::Lowerer::visitCall(IR::Instrs::Call *instr)
{
    std::vector<llvm::Value*> args;
    args.reserve(instr->args.size());
    for (IR::ASTValue const &v : instr->args)
        args.push_back(lower(v));

    llvm::Function *callee = static_cast<llvm::Function*>(lower(instr->f));
    llvm::Value *res = builder.CreateCall(callee, args);

    if (!dynamic_cast<IR::VoidType*>(instr->reg->type()))
        tempregisters[instr->reg] = res;
}
