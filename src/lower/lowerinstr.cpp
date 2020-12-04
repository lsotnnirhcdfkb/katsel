#include "ir/instruction.h"
#include "ir/visitor.h"
#include "ir/type.h"
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

#define DEF_BIN_INSTR(name, ifFloating, ifSignedInt, ifUnsignedInt)                                                               \
    void Lower::Lowerer::visit##name(IR::Instrs::name *instr)                                                                     \
    {                                                                                                                             \
        IR::BuiltinType *bty = static_cast<IR::BuiltinType*>(instr->lhs.type());                                                  \
        if (bty->isFloating())                                                                                                    \
            builder.CreateStore(builder.Create##ifFloating (lower(instr->lhs), lower(instr->rhs)), allocas.at(instr->target));    \
        else if (bty->isSigned())                                                                                                 \
            builder.CreateStore(builder.Create##ifSignedInt (lower(instr->lhs), lower(instr->rhs)), allocas.at(instr->target));   \
        else                                                                                                                      \
            builder.CreateStore(builder.Create##ifUnsignedInt (lower(instr->lhs), lower(instr->rhs)), allocas.at(instr->target)); \
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
    builder.CreateStore(builder.CreateXor(llvm::ConstantInt::get(instr->op.type()->toLLVMType(context), -1), lower(instr->op)), allocas.at(instr->target));
}
void Lower::Lowerer::visitShiftR(IR::Instrs::ShiftR *instr)
{
    builder.CreateStore(builder.CreateLShr(lower(instr->lhs), lower(instr->rhs)), allocas.at(instr->target));
}
void Lower::Lowerer::visitShiftL(IR::Instrs::ShiftL *instr)
{
    builder.CreateStore(builder.CreateShl(lower(instr->lhs), lower(instr->rhs)), allocas.at(instr->target));
}
void Lower::Lowerer::visitNeg(IR::Instrs::Neg *instr)
{
    IR::BuiltinType *bty = static_cast<IR::BuiltinType*>(instr->op.type());
    if (bty->isFloating())
        builder.CreateStore(builder.CreateFNeg(lower(instr->op)), allocas.at(instr->target));
    else
        builder.CreateStore(builder.CreateSub(llvm::ConstantInt::get(instr->op.type()->toLLVMType(context), 0), lower(instr->op)), allocas.at(instr->target));
}
void Lower::Lowerer::visitTrunc(IR::Instrs::Trunc *instr)
{
    IR::BuiltinType *bty = static_cast<IR::BuiltinType*>(instr->op.type());
    if (bty->isFloating())
        builder.CreateStore(builder.CreateFPTrunc(lower(instr->op), instr->newt->toLLVMType(context)), allocas.at(instr->target));
    else
        builder.CreateStore(builder.CreateTrunc(lower(instr->op), instr->newt->toLLVMType(context)), allocas.at(instr->target));
}
void Lower::Lowerer::visitExt(IR::Instrs::Ext *instr)
{
    IR::BuiltinType *bty = static_cast<IR::BuiltinType*>(instr->op.type());
    if (bty->isFloating())
        builder.CreateStore(builder.CreateFPExt(lower(instr->op), instr->newt->toLLVMType(context)), allocas.at(instr->target));
    else if (bty->isSigned())
        builder.CreateStore(builder.CreateSExt(lower(instr->op), instr->newt->toLLVMType(context)), allocas.at(instr->target));
    else
        builder.CreateStore(builder.CreateZExt(lower(instr->op), instr->newt->toLLVMType(context)), allocas.at(instr->target));
}
void Lower::Lowerer::visitIntToFloat(IR::Instrs::IntToFloat *instr)
{
    IR::BuiltinType *bty = static_cast<IR::BuiltinType*>(instr->op.type());
    if (bty->isSigned())
        builder.CreateStore(builder.CreateSIToFP(lower(instr->op), instr->newt->toLLVMType(context)), allocas.at(instr->target));
    else
        builder.CreateStore(builder.CreateUIToFP(lower(instr->op), instr->newt->toLLVMType(context)), allocas.at(instr->target));
}
void Lower::Lowerer::visitFloatToInt(IR::Instrs::FloatToInt *instr)
{
    if (instr->newt->isSigned())
        builder.CreateStore(builder.CreateFPToSI(lower(instr->op), instr->newt->toLLVMType(context)), allocas.at(instr->target));
    else
        builder.CreateStore(builder.CreateFPToUI(lower(instr->op), instr->newt->toLLVMType(context)), allocas.at(instr->target));
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
        builder.CreateStore(res, allocas.at(instr->reg));
}
