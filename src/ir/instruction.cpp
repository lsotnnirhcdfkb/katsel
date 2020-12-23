#include "ir/instruction.h"
#include "ir/value.h"
#include "ir/type.h"
#include "ir/visitor.h"
#include "llvm/Support/raw_ostream.h"
#include "utils/assert.h"

// INSTR CPP START
// The following code was autogenerated - see the utils/ directory
IR::Instrs::Store::Store(Register *target, ASTValue value): target(target), value(value)
{
    ASSERT(target->type() == value.type())
}
void IR::Instrs::Store::accept(InstructionVisitor *v) { v->visitStore(this); }
IR::Instrs::Phi::Phi(TempRegister *target, std::vector<std::pair<Block*,ASTValue>> prevs): target(target), prevs(prevs)
{
}
void IR::Instrs::Phi::accept(InstructionVisitor *v) { v->visitPhi(this); }
IR::Instrs::Or::Or(TempRegister *target, ASTValue lhs, ASTValue rhs): target(target), lhs(lhs), rhs(rhs)
{
    ASSERT(dynamic_cast<BoolType*>(target->type()))
    ASSERT(target->type() == lhs.type())
    ASSERT(dynamic_cast<BoolType*>(lhs.type()))
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::Or::accept(InstructionVisitor *v) { v->visitOr(this); }
IR::Instrs::And::And(TempRegister *target, ASTValue lhs, ASTValue rhs): target(target), lhs(lhs), rhs(rhs)
{
    ASSERT(dynamic_cast<BoolType*>(target->type()))
    ASSERT(target->type() == lhs.type())
    ASSERT(dynamic_cast<BoolType*>(lhs.type()))
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::And::accept(InstructionVisitor *v) { v->visitAnd(this); }
IR::Instrs::Not::Not(TempRegister *target, ASTValue op): target(target), op(op)
{
    ASSERT(dynamic_cast<BoolType*>(target->type()))
    ASSERT(target->type() == op.type())
}
void IR::Instrs::Not::accept(InstructionVisitor *v) { v->visitNot(this); }
IR::Instrs::ICmpNE::ICmpNE(TempRegister *target, ASTValue lhs, ASTValue rhs): target(target), lhs(lhs), rhs(rhs)
{
    ASSERT(dynamic_cast<BoolType*>(target->type()))
    ASSERT(dynamic_cast<IntType*>(lhs.type()) || dynamic_cast<GenericIntType*>(lhs.type()))
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::ICmpNE::accept(InstructionVisitor *v) { v->visitICmpNE(this); }
IR::Instrs::ICmpEQ::ICmpEQ(TempRegister *target, ASTValue lhs, ASTValue rhs): target(target), lhs(lhs), rhs(rhs)
{
    ASSERT(dynamic_cast<BoolType*>(target->type()))
    ASSERT(dynamic_cast<IntType*>(lhs.type()) || dynamic_cast<GenericIntType*>(lhs.type()))
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::ICmpEQ::accept(InstructionVisitor *v) { v->visitICmpEQ(this); }
IR::Instrs::ICmpLT::ICmpLT(TempRegister *target, ASTValue lhs, ASTValue rhs): target(target), lhs(lhs), rhs(rhs)
{
    ASSERT(dynamic_cast<BoolType*>(target->type()))
    ASSERT(dynamic_cast<IntType*>(lhs.type()) || dynamic_cast<GenericIntType*>(lhs.type()))
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::ICmpLT::accept(InstructionVisitor *v) { v->visitICmpLT(this); }
IR::Instrs::ICmpGT::ICmpGT(TempRegister *target, ASTValue lhs, ASTValue rhs): target(target), lhs(lhs), rhs(rhs)
{
    ASSERT(dynamic_cast<BoolType*>(target->type()))
    ASSERT(dynamic_cast<IntType*>(lhs.type()) || dynamic_cast<GenericIntType*>(lhs.type()))
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::ICmpGT::accept(InstructionVisitor *v) { v->visitICmpGT(this); }
IR::Instrs::ICmpLE::ICmpLE(TempRegister *target, ASTValue lhs, ASTValue rhs): target(target), lhs(lhs), rhs(rhs)
{
    ASSERT(dynamic_cast<BoolType*>(target->type()))
    ASSERT(dynamic_cast<IntType*>(lhs.type()) || dynamic_cast<GenericIntType*>(lhs.type()))
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::ICmpLE::accept(InstructionVisitor *v) { v->visitICmpLE(this); }
IR::Instrs::ICmpGE::ICmpGE(TempRegister *target, ASTValue lhs, ASTValue rhs): target(target), lhs(lhs), rhs(rhs)
{
    ASSERT(dynamic_cast<BoolType*>(target->type()))
    ASSERT(dynamic_cast<IntType*>(lhs.type()) || dynamic_cast<GenericIntType*>(lhs.type()))
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::ICmpGE::accept(InstructionVisitor *v) { v->visitICmpGE(this); }
IR::Instrs::IAdd::IAdd(TempRegister *target, ASTValue lhs, ASTValue rhs): target(target), lhs(lhs), rhs(rhs)
{
    ASSERT(dynamic_cast<IntType*>(target->type()) || dynamic_cast<GenericIntType*>(target->type()))
    ASSERT(target->type() == lhs.type())
    ASSERT(dynamic_cast<IntType*>(lhs.type()) || dynamic_cast<GenericIntType*>(lhs.type()))
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::IAdd::accept(InstructionVisitor *v) { v->visitIAdd(this); }
IR::Instrs::ISub::ISub(TempRegister *target, ASTValue lhs, ASTValue rhs): target(target), lhs(lhs), rhs(rhs)
{
    ASSERT(dynamic_cast<IntType*>(target->type()) || dynamic_cast<GenericIntType*>(target->type()))
    ASSERT(target->type() == lhs.type())
    ASSERT(dynamic_cast<IntType*>(lhs.type()) || dynamic_cast<GenericIntType*>(lhs.type()))
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::ISub::accept(InstructionVisitor *v) { v->visitISub(this); }
IR::Instrs::IMult::IMult(TempRegister *target, ASTValue lhs, ASTValue rhs): target(target), lhs(lhs), rhs(rhs)
{
    ASSERT(dynamic_cast<IntType*>(target->type()) || dynamic_cast<GenericIntType*>(target->type()))
    ASSERT(target->type() == lhs.type())
    ASSERT(dynamic_cast<IntType*>(lhs.type()) || dynamic_cast<GenericIntType*>(lhs.type()))
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::IMult::accept(InstructionVisitor *v) { v->visitIMult(this); }
IR::Instrs::IDiv::IDiv(TempRegister *target, ASTValue lhs, ASTValue rhs): target(target), lhs(lhs), rhs(rhs)
{
    ASSERT(dynamic_cast<IntType*>(target->type()) || dynamic_cast<GenericIntType*>(target->type()))
    ASSERT(target->type() == lhs.type())
    ASSERT(dynamic_cast<IntType*>(lhs.type()) || dynamic_cast<GenericIntType*>(lhs.type()))
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::IDiv::accept(InstructionVisitor *v) { v->visitIDiv(this); }
IR::Instrs::IMod::IMod(TempRegister *target, ASTValue lhs, ASTValue rhs): target(target), lhs(lhs), rhs(rhs)
{
    ASSERT(dynamic_cast<IntType*>(target->type()) || dynamic_cast<GenericIntType*>(target->type()))
    ASSERT(target->type() == lhs.type())
    ASSERT(dynamic_cast<IntType*>(lhs.type()) || dynamic_cast<GenericIntType*>(lhs.type()))
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::IMod::accept(InstructionVisitor *v) { v->visitIMod(this); }
IR::Instrs::INeg::INeg(TempRegister *target, ASTValue op): target(target), op(op)
{
    ASSERT(dynamic_cast<IntType*>(target->type()) || dynamic_cast<GenericIntType*>(target->type()))
    ASSERT(target->type() == op.type())
    ASSERT(dynamic_cast<IntType*>(op.type()) || dynamic_cast<GenericIntType*>(op.type()))
}
void IR::Instrs::INeg::accept(InstructionVisitor *v) { v->visitINeg(this); }
IR::Instrs::FCmpNE::FCmpNE(TempRegister *target, ASTValue lhs, ASTValue rhs): target(target), lhs(lhs), rhs(rhs)
{
    ASSERT(dynamic_cast<BoolType*>(target->type()))
    ASSERT(dynamic_cast<FloatType*>(lhs.type()) || dynamic_cast<GenericFloatType*>(lhs.type()))
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::FCmpNE::accept(InstructionVisitor *v) { v->visitFCmpNE(this); }
IR::Instrs::FCmpEQ::FCmpEQ(TempRegister *target, ASTValue lhs, ASTValue rhs): target(target), lhs(lhs), rhs(rhs)
{
    ASSERT(dynamic_cast<BoolType*>(target->type()))
    ASSERT(dynamic_cast<FloatType*>(lhs.type()) || dynamic_cast<GenericFloatType*>(lhs.type()))
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::FCmpEQ::accept(InstructionVisitor *v) { v->visitFCmpEQ(this); }
IR::Instrs::FCmpLT::FCmpLT(TempRegister *target, ASTValue lhs, ASTValue rhs): target(target), lhs(lhs), rhs(rhs)
{
    ASSERT(dynamic_cast<BoolType*>(target->type()))
    ASSERT(dynamic_cast<FloatType*>(lhs.type()) || dynamic_cast<GenericFloatType*>(lhs.type()))
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::FCmpLT::accept(InstructionVisitor *v) { v->visitFCmpLT(this); }
IR::Instrs::FCmpGT::FCmpGT(TempRegister *target, ASTValue lhs, ASTValue rhs): target(target), lhs(lhs), rhs(rhs)
{
    ASSERT(dynamic_cast<BoolType*>(target->type()))
    ASSERT(dynamic_cast<FloatType*>(lhs.type()) || dynamic_cast<GenericFloatType*>(lhs.type()))
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::FCmpGT::accept(InstructionVisitor *v) { v->visitFCmpGT(this); }
IR::Instrs::FCmpLE::FCmpLE(TempRegister *target, ASTValue lhs, ASTValue rhs): target(target), lhs(lhs), rhs(rhs)
{
    ASSERT(dynamic_cast<BoolType*>(target->type()))
    ASSERT(dynamic_cast<FloatType*>(lhs.type()) || dynamic_cast<GenericFloatType*>(lhs.type()))
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::FCmpLE::accept(InstructionVisitor *v) { v->visitFCmpLE(this); }
IR::Instrs::FCmpGE::FCmpGE(TempRegister *target, ASTValue lhs, ASTValue rhs): target(target), lhs(lhs), rhs(rhs)
{
    ASSERT(dynamic_cast<BoolType*>(target->type()))
    ASSERT(dynamic_cast<FloatType*>(lhs.type()) || dynamic_cast<GenericFloatType*>(lhs.type()))
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::FCmpGE::accept(InstructionVisitor *v) { v->visitFCmpGE(this); }
IR::Instrs::FAdd::FAdd(TempRegister *target, ASTValue lhs, ASTValue rhs): target(target), lhs(lhs), rhs(rhs)
{
    ASSERT(dynamic_cast<FloatType*>(target->type()) || dynamic_cast<GenericFloatType*>(target->type()))
    ASSERT(target->type() == lhs.type())
    ASSERT(dynamic_cast<FloatType*>(lhs.type()) || dynamic_cast<GenericFloatType*>(lhs.type()))
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::FAdd::accept(InstructionVisitor *v) { v->visitFAdd(this); }
IR::Instrs::FSub::FSub(TempRegister *target, ASTValue lhs, ASTValue rhs): target(target), lhs(lhs), rhs(rhs)
{
    ASSERT(dynamic_cast<FloatType*>(target->type()) || dynamic_cast<GenericFloatType*>(target->type()))
    ASSERT(target->type() == lhs.type())
    ASSERT(dynamic_cast<FloatType*>(lhs.type()) || dynamic_cast<GenericFloatType*>(lhs.type()))
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::FSub::accept(InstructionVisitor *v) { v->visitFSub(this); }
IR::Instrs::FMult::FMult(TempRegister *target, ASTValue lhs, ASTValue rhs): target(target), lhs(lhs), rhs(rhs)
{
    ASSERT(dynamic_cast<FloatType*>(target->type()) || dynamic_cast<GenericFloatType*>(target->type()))
    ASSERT(target->type() == lhs.type())
    ASSERT(dynamic_cast<FloatType*>(lhs.type()) || dynamic_cast<GenericFloatType*>(lhs.type()))
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::FMult::accept(InstructionVisitor *v) { v->visitFMult(this); }
IR::Instrs::FDiv::FDiv(TempRegister *target, ASTValue lhs, ASTValue rhs): target(target), lhs(lhs), rhs(rhs)
{
    ASSERT(dynamic_cast<FloatType*>(target->type()) || dynamic_cast<GenericFloatType*>(target->type()))
    ASSERT(target->type() == lhs.type())
    ASSERT(dynamic_cast<FloatType*>(lhs.type()) || dynamic_cast<GenericFloatType*>(lhs.type()))
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::FDiv::accept(InstructionVisitor *v) { v->visitFDiv(this); }
IR::Instrs::FMod::FMod(TempRegister *target, ASTValue lhs, ASTValue rhs): target(target), lhs(lhs), rhs(rhs)
{
    ASSERT(dynamic_cast<FloatType*>(target->type()) || dynamic_cast<GenericFloatType*>(target->type()))
    ASSERT(target->type() == lhs.type())
    ASSERT(dynamic_cast<FloatType*>(lhs.type()) || dynamic_cast<GenericFloatType*>(lhs.type()))
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::FMod::accept(InstructionVisitor *v) { v->visitFMod(this); }
IR::Instrs::FNeg::FNeg(TempRegister *target, ASTValue op): target(target), op(op)
{
    ASSERT(dynamic_cast<FloatType*>(target->type()) || dynamic_cast<GenericFloatType*>(target->type()))
    ASSERT(target->type() == op.type())
    ASSERT(dynamic_cast<FloatType*>(op.type()) || dynamic_cast<GenericFloatType*>(op.type()))
}
void IR::Instrs::FNeg::accept(InstructionVisitor *v) { v->visitFNeg(this); }
IR::Instrs::BitXor::BitXor(TempRegister *target, ASTValue lhs, ASTValue rhs): target(target), lhs(lhs), rhs(rhs)
{
    ASSERT(dynamic_cast<IntType*>(target->type()) || dynamic_cast<GenericIntType*>(target->type()))
    ASSERT(dynamic_cast<IntType*>(lhs.type()) || dynamic_cast<GenericIntType*>(lhs.type()))
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::BitXor::accept(InstructionVisitor *v) { v->visitBitXor(this); }
IR::Instrs::BitOr::BitOr(TempRegister *target, ASTValue lhs, ASTValue rhs): target(target), lhs(lhs), rhs(rhs)
{
    ASSERT(dynamic_cast<IntType*>(target->type()) || dynamic_cast<GenericIntType*>(target->type()))
    ASSERT(dynamic_cast<IntType*>(lhs.type()) || dynamic_cast<GenericIntType*>(lhs.type()))
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::BitOr::accept(InstructionVisitor *v) { v->visitBitOr(this); }
IR::Instrs::BitAnd::BitAnd(TempRegister *target, ASTValue lhs, ASTValue rhs): target(target), lhs(lhs), rhs(rhs)
{
    ASSERT(dynamic_cast<IntType*>(target->type()) || dynamic_cast<GenericIntType*>(target->type()))
    ASSERT(dynamic_cast<IntType*>(lhs.type()) || dynamic_cast<GenericIntType*>(lhs.type()))
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::BitAnd::accept(InstructionVisitor *v) { v->visitBitAnd(this); }
IR::Instrs::BitNot::BitNot(TempRegister *target, ASTValue op): target(target), op(op)
{
    ASSERT(dynamic_cast<IntType*>(target->type()) || dynamic_cast<GenericIntType*>(target->type()))
    ASSERT(dynamic_cast<IntType*>(op.type()) || dynamic_cast<GenericIntType*>(op.type()))
}
void IR::Instrs::BitNot::accept(InstructionVisitor *v) { v->visitBitNot(this); }
IR::Instrs::ShiftR::ShiftR(TempRegister *target, ASTValue lhs, ASTValue rhs): target(target), lhs(lhs), rhs(rhs)
{
    ASSERT(dynamic_cast<IntType*>(target->type()) || dynamic_cast<GenericIntType*>(target->type()))
    ASSERT(target->type() == lhs.type())
}
void IR::Instrs::ShiftR::accept(InstructionVisitor *v) { v->visitShiftR(this); }
IR::Instrs::ShiftL::ShiftL(TempRegister *target, ASTValue lhs, ASTValue rhs): target(target), lhs(lhs), rhs(rhs)
{
    ASSERT(dynamic_cast<IntType*>(target->type()) || dynamic_cast<GenericIntType*>(target->type()))
    ASSERT(target->type() == lhs.type())
}
void IR::Instrs::ShiftL::accept(InstructionVisitor *v) { v->visitShiftL(this); }
IR::Instrs::NoOpCast::NoOpCast(TempRegister *target, ASTValue op, Type *newt): target(target), op(op), newt(newt)
{
}
void IR::Instrs::NoOpCast::accept(InstructionVisitor *v) { v->visitNoOpCast(this); }
IR::Instrs::IntToInt::IntToInt(TempRegister *target, ASTValue op, IntType *newt): target(target), op(op), newt(newt)
{
    ASSERT(dynamic_cast<IntType*>(op.type()) || dynamic_cast<GenericIntType*>(op.type()))
    ASSERT(target->type() == newt)
}
void IR::Instrs::IntToInt::accept(InstructionVisitor *v) { v->visitIntToInt(this); }
IR::Instrs::IntToFloat::IntToFloat(TempRegister *target, ASTValue op, FloatType *newt): target(target), op(op), newt(newt)
{
    ASSERT(dynamic_cast<IntType*>(op.type()) || dynamic_cast<GenericIntType*>(op.type()))
    ASSERT(target->type() == newt)
}
void IR::Instrs::IntToFloat::accept(InstructionVisitor *v) { v->visitIntToFloat(this); }
IR::Instrs::FloatToFloat::FloatToFloat(TempRegister *target, ASTValue op, FloatType *newt): target(target), op(op), newt(newt)
{
    ASSERT(dynamic_cast<FloatType*>(op.type()) || dynamic_cast<GenericFloatType*>(op.type()))
    ASSERT(target->type() == newt)
}
void IR::Instrs::FloatToFloat::accept(InstructionVisitor *v) { v->visitFloatToFloat(this); }
IR::Instrs::FloatToInt::FloatToInt(TempRegister *target, ASTValue op, IntType *newt): target(target), op(op), newt(newt)
{
    ASSERT(dynamic_cast<FloatType*>(op.type()) || dynamic_cast<GenericFloatType*>(op.type()))
    ASSERT(target->type() == newt)
}
void IR::Instrs::FloatToInt::accept(InstructionVisitor *v) { v->visitFloatToInt(this); }
IR::Instrs::Call::Call(TempRegister *target, Function *f, std::vector<ASTValue> args): target(target), f(f), args(args)
{
    ASSERT(target->type() == f->ty->ret)
    ASSERT(args.size() == f->ty->paramtys.size())
}
void IR::Instrs::Call::accept(InstructionVisitor *v) { v->visitCall(this); }
IR::Instrs::Return::Return(Register *value): value(value)
{
}
void IR::Instrs::Return::accept(BrVisitor *v) { v->visitReturn(this); }
IR::Instrs::GotoBr::GotoBr(Block *to): to(to)
{
}
void IR::Instrs::GotoBr::accept(BrVisitor *v) { v->visitGotoBr(this); }
IR::Instrs::CondBr::CondBr(ASTValue v, Block *trueB, Block *falseB): v(v), trueB(trueB), falseB(falseB)
{
    ASSERT(dynamic_cast<BoolType*>(v.type()))
}
void IR::Instrs::CondBr::accept(BrVisitor *v) { v->visitCondBr(this); }
// This code was autogenerated - see the utils/ directory
// INSTR CPP END
