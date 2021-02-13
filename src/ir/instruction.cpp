#include "ir/instruction.h"
#include "ir/function.h"
#include "ir/value.h"
#include "ir/type.h"
#include "ir/visitor.h"
#include "utils/assert.h"
#include "utils/format.h"
#include "codegen/context.h"

// INSTR CPP START
IR::Instrs::Copy::Copy(IR::Register& target, Located<NNPtr<Value>> val): target(target), val(val) {
    ASSERT(&target.type() == &val.value->type())
}
void IR::Instrs::Copy::accept(InstructionVisitor &v) const { v.visit(*this); }

IR::Instrs::Or::Or(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): target(target), lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<BoolType const *>(&target.type()))
    ASSERT(dynamic_cast<BoolType const *>(&lhs.value->type()))
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::Or::accept(InstructionVisitor &v) const { v.visit(*this); }

IR::Instrs::And::And(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): target(target), lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<BoolType const *>(&target.type()))
    ASSERT(dynamic_cast<BoolType const *>(&lhs.value->type()))
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::And::accept(InstructionVisitor &v) const { v.visit(*this); }

IR::Instrs::Not::Not(IR::Register& target, Located<NNPtr<Value>> op): target(target), op(op) {
    ASSERT(dynamic_cast<BoolType const *>(&target.type()))
}
void IR::Instrs::Not::accept(InstructionVisitor &v) const { v.visit(*this); }

IR::Instrs::ICmpNE::ICmpNE(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): target(target), lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<BoolType const *>(&target.type()))
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::ICmpNE::accept(InstructionVisitor &v) const { v.visit(*this); }

IR::Instrs::ICmpEQ::ICmpEQ(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): target(target), lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<BoolType const *>(&target.type()))
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::ICmpEQ::accept(InstructionVisitor &v) const { v.visit(*this); }

IR::Instrs::ICmpLT::ICmpLT(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): target(target), lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<BoolType const *>(&target.type()))
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::ICmpLT::accept(InstructionVisitor &v) const { v.visit(*this); }

IR::Instrs::ICmpGT::ICmpGT(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): target(target), lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<BoolType const *>(&target.type()))
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::ICmpGT::accept(InstructionVisitor &v) const { v.visit(*this); }

IR::Instrs::ICmpLE::ICmpLE(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): target(target), lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<BoolType const *>(&target.type()))
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::ICmpLE::accept(InstructionVisitor &v) const { v.visit(*this); }

IR::Instrs::ICmpGE::ICmpGE(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): target(target), lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<BoolType const *>(&target.type()))
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::ICmpGE::accept(InstructionVisitor &v) const { v.visit(*this); }

IR::Instrs::IAdd::IAdd(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): target(target), lhs(lhs), rhs(rhs) {
    ASSERT(&target.type() == &lhs.value->type())
    ASSERT(dynamic_cast<IntType const *>(&lhs.value->type()) || dynamic_cast<GenericIntType const *>(&lhs.value->type()))
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::IAdd::accept(InstructionVisitor &v) const { v.visit(*this); }

IR::Instrs::ISub::ISub(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): target(target), lhs(lhs), rhs(rhs) {
    ASSERT(&target.type() == &lhs.value->type())
    ASSERT(dynamic_cast<IntType const *>(&lhs.value->type()) || dynamic_cast<GenericIntType const *>(&lhs.value->type()))
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::ISub::accept(InstructionVisitor &v) const { v.visit(*this); }

IR::Instrs::IMult::IMult(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): target(target), lhs(lhs), rhs(rhs) {
    ASSERT(&target.type() == &lhs.value->type())
    ASSERT(dynamic_cast<IntType const *>(&lhs.value->type()) || dynamic_cast<GenericIntType const *>(&lhs.value->type()))
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::IMult::accept(InstructionVisitor &v) const { v.visit(*this); }

IR::Instrs::IDiv::IDiv(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): target(target), lhs(lhs), rhs(rhs) {
    ASSERT(&target.type() == &lhs.value->type())
    ASSERT(dynamic_cast<IntType const *>(&lhs.value->type()) || dynamic_cast<GenericIntType const *>(&lhs.value->type()))
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::IDiv::accept(InstructionVisitor &v) const { v.visit(*this); }

IR::Instrs::IMod::IMod(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): target(target), lhs(lhs), rhs(rhs) {
    ASSERT(&target.type() == &lhs.value->type())
    ASSERT(dynamic_cast<IntType const *>(&lhs.value->type()) || dynamic_cast<GenericIntType const *>(&lhs.value->type()))
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::IMod::accept(InstructionVisitor &v) const { v.visit(*this); }

IR::Instrs::INeg::INeg(IR::Register& target, Located<NNPtr<Value>> op): target(target), op(op) {
    ASSERT(&target.type() == &op.value->type())
    ASSERT(dynamic_cast<IntType const *>(&op.value->type()) || dynamic_cast<GenericIntType const *>(&op.value->type()))
}
void IR::Instrs::INeg::accept(InstructionVisitor &v) const { v.visit(*this); }

IR::Instrs::FCmpNE::FCmpNE(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): target(target), lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<BoolType const *>(&target.type()))
    ASSERT(dynamic_cast<FloatType const *>(&lhs.value->type()) || dynamic_cast<GenericFloatType const *>(&lhs.value->type()))
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::FCmpNE::accept(InstructionVisitor &v) const { v.visit(*this); }

IR::Instrs::FCmpEQ::FCmpEQ(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): target(target), lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<BoolType const *>(&target.type()))
    ASSERT(dynamic_cast<FloatType const *>(&lhs.value->type()) || dynamic_cast<GenericFloatType const *>(&lhs.value->type()))
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::FCmpEQ::accept(InstructionVisitor &v) const { v.visit(*this); }

IR::Instrs::FCmpLT::FCmpLT(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): target(target), lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<BoolType const *>(&target.type()))
    ASSERT(dynamic_cast<FloatType const *>(&lhs.value->type()) || dynamic_cast<GenericFloatType const *>(&lhs.value->type()))
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::FCmpLT::accept(InstructionVisitor &v) const { v.visit(*this); }

IR::Instrs::FCmpGT::FCmpGT(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): target(target), lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<BoolType const *>(&target.type()))
    ASSERT(dynamic_cast<FloatType const *>(&lhs.value->type()) || dynamic_cast<GenericFloatType const *>(&lhs.value->type()))
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::FCmpGT::accept(InstructionVisitor &v) const { v.visit(*this); }

IR::Instrs::FCmpLE::FCmpLE(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): target(target), lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<BoolType const *>(&target.type()))
    ASSERT(dynamic_cast<FloatType const *>(&lhs.value->type()) || dynamic_cast<GenericFloatType const *>(&lhs.value->type()))
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::FCmpLE::accept(InstructionVisitor &v) const { v.visit(*this); }

IR::Instrs::FCmpGE::FCmpGE(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): target(target), lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<BoolType const *>(&target.type()))
    ASSERT(dynamic_cast<FloatType const *>(&lhs.value->type()) || dynamic_cast<GenericFloatType const *>(&lhs.value->type()))
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::FCmpGE::accept(InstructionVisitor &v) const { v.visit(*this); }

IR::Instrs::FAdd::FAdd(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): target(target), lhs(lhs), rhs(rhs) {
    ASSERT(&target.type() == &lhs.value->type())
    ASSERT(dynamic_cast<FloatType const *>(&lhs.value->type()) || dynamic_cast<GenericFloatType const *>(&lhs.value->type()))
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::FAdd::accept(InstructionVisitor &v) const { v.visit(*this); }

IR::Instrs::FSub::FSub(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): target(target), lhs(lhs), rhs(rhs) {
    ASSERT(&target.type() == &lhs.value->type())
    ASSERT(dynamic_cast<FloatType const *>(&lhs.value->type()) || dynamic_cast<GenericFloatType const *>(&lhs.value->type()))
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::FSub::accept(InstructionVisitor &v) const { v.visit(*this); }

IR::Instrs::FMult::FMult(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): target(target), lhs(lhs), rhs(rhs) {
    ASSERT(&target.type() == &lhs.value->type())
    ASSERT(dynamic_cast<FloatType const *>(&lhs.value->type()) || dynamic_cast<GenericFloatType const *>(&lhs.value->type()))
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::FMult::accept(InstructionVisitor &v) const { v.visit(*this); }

IR::Instrs::FDiv::FDiv(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): target(target), lhs(lhs), rhs(rhs) {
    ASSERT(&target.type() == &lhs.value->type())
    ASSERT(dynamic_cast<FloatType const *>(&lhs.value->type()) || dynamic_cast<GenericFloatType const *>(&lhs.value->type()))
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::FDiv::accept(InstructionVisitor &v) const { v.visit(*this); }

IR::Instrs::FMod::FMod(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): target(target), lhs(lhs), rhs(rhs) {
    ASSERT(&target.type() == &lhs.value->type())
    ASSERT(dynamic_cast<FloatType const *>(&lhs.value->type()) || dynamic_cast<GenericFloatType const *>(&lhs.value->type()))
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::FMod::accept(InstructionVisitor &v) const { v.visit(*this); }

IR::Instrs::FNeg::FNeg(IR::Register& target, Located<NNPtr<Value>> op): target(target), op(op) {
    ASSERT(&target.type() == &op.value->type())
    ASSERT(dynamic_cast<FloatType const *>(&op.value->type()) || dynamic_cast<GenericFloatType const *>(&op.value->type()))
}
void IR::Instrs::FNeg::accept(InstructionVisitor &v) const { v.visit(*this); }

IR::Instrs::BitXor::BitXor(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): target(target), lhs(lhs), rhs(rhs) {
    ASSERT(&target.type() == &lhs.value->type())
    ASSERT(dynamic_cast<IntType const *>(&lhs.value->type()) || dynamic_cast<GenericIntType const *>(&lhs.value->type()))
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::BitXor::accept(InstructionVisitor &v) const { v.visit(*this); }

IR::Instrs::BitOr::BitOr(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): target(target), lhs(lhs), rhs(rhs) {
    ASSERT(&target.type() == &lhs.value->type())
    ASSERT(dynamic_cast<IntType const *>(&lhs.value->type()) || dynamic_cast<GenericIntType const *>(&lhs.value->type()))
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::BitOr::accept(InstructionVisitor &v) const { v.visit(*this); }

IR::Instrs::BitAnd::BitAnd(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): target(target), lhs(lhs), rhs(rhs) {
    ASSERT(&target.type() == &lhs.value->type())
    ASSERT(dynamic_cast<IntType const *>(&lhs.value->type()) || dynamic_cast<GenericIntType const *>(&lhs.value->type()))
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::BitAnd::accept(InstructionVisitor &v) const { v.visit(*this); }

IR::Instrs::BitNot::BitNot(IR::Register& target, Located<NNPtr<Value>> op): target(target), op(op) {
    ASSERT(&target.type() == &op.value->type())
    ASSERT(dynamic_cast<IntType const *>(&op.value->type()) || dynamic_cast<GenericIntType const *>(&op.value->type()))
}
void IR::Instrs::BitNot::accept(InstructionVisitor &v) const { v.visit(*this); }

IR::Instrs::ShiftR::ShiftR(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): target(target), lhs(lhs), rhs(rhs) {
    ASSERT(&target.type() == &lhs.value->type())
    ASSERT(dynamic_cast<IntType const *>(&lhs.value->type()) || dynamic_cast<GenericIntType const *>(&lhs.value->type()))
    ASSERT(dynamic_cast<IntType const *>(&lhs.value->type()) || dynamic_cast<GenericIntType const *>(&lhs.value->type()))
}
void IR::Instrs::ShiftR::accept(InstructionVisitor &v) const { v.visit(*this); }

IR::Instrs::ShiftL::ShiftL(IR::Register& target, Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): target(target), lhs(lhs), rhs(rhs) {
    ASSERT(&target.type() == &lhs.value->type())
    ASSERT(dynamic_cast<IntType const *>(&lhs.value->type()) || dynamic_cast<GenericIntType const *>(&lhs.value->type()))
    ASSERT(dynamic_cast<IntType const *>(&lhs.value->type()) || dynamic_cast<GenericIntType const *>(&lhs.value->type()))
}
void IR::Instrs::ShiftL::accept(InstructionVisitor &v) const { v.visit(*this); }

IR::Instrs::NoOpCast::NoOpCast(IR::Register& target, Located<NNPtr<Value>> op, NNPtr<Type const> newt): target(target), op(op), newt(newt) {
    ASSERT(&target.type() == &*newt)
}
void IR::Instrs::NoOpCast::accept(InstructionVisitor &v) const { v.visit(*this); }

IR::Instrs::IntToInt::IntToInt(IR::Register& target, Located<NNPtr<Value>> op, NNPtr<IntType const> newt): target(target), op(op), newt(newt) {
    ASSERT(&target.type() == &*newt)
    ASSERT(dynamic_cast<IntType const *>(&op.value->type()) || dynamic_cast<GenericIntType const *>(&op.value->type()))
}
void IR::Instrs::IntToInt::accept(InstructionVisitor &v) const { v.visit(*this); }

IR::Instrs::IntToFloat::IntToFloat(IR::Register& target, Located<NNPtr<Value>> op, NNPtr<FloatType const> newt): target(target), op(op), newt(newt) {
    ASSERT(&target.type() == &*newt)
    ASSERT(dynamic_cast<IntType const *>(&op.value->type()) || dynamic_cast<GenericIntType const *>(&op.value->type()))
}
void IR::Instrs::IntToFloat::accept(InstructionVisitor &v) const { v.visit(*this); }

IR::Instrs::FloatToFloat::FloatToFloat(IR::Register& target, Located<NNPtr<Value>> op, NNPtr<FloatType const> newt): target(target), op(op), newt(newt) {
    ASSERT(&target.type() == &*newt)
    ASSERT(dynamic_cast<FloatType const *>(&op.value->type()) || dynamic_cast<GenericFloatType const *>(&op.value->type()))
}
void IR::Instrs::FloatToFloat::accept(InstructionVisitor &v) const { v.visit(*this); }

IR::Instrs::FloatToInt::FloatToInt(IR::Register& target, Located<NNPtr<Value>> op, NNPtr<IntType const> newt): target(target), op(op), newt(newt) {
    ASSERT(&target.type() == &*newt)
    ASSERT(dynamic_cast<FloatType const *>(&op.value->type()) || dynamic_cast<GenericFloatType const *>(&op.value->type()))
}
void IR::Instrs::FloatToInt::accept(InstructionVisitor &v) const { v.visit(*this); }

IR::Instrs::Call::Call(IR::Register& target, NNPtr<Function const> f, std::vector<Located<NNPtr<Value>>> args): target(target), f(f), args(args) {
    ASSERT(&target.type() == &*f->ty->ret)
    ASSERT(args.size() == f->ty->paramtys.size())
}
void IR::Instrs::Call::accept(InstructionVisitor &v) const { v.visit(*this); }

IR::Instrs::Addrof::Addrof(IR::Register& target, IR::Register& reg, bool mut): target(target), reg(reg), mut(mut) {
    ASSERT(static_cast<int>(mut) <= static_cast<int>(reg.mut))
}
void IR::Instrs::Addrof::accept(InstructionVisitor &v) const { v.visit(*this); }

IR::Instrs::DerefPtr::DerefPtr(IR::Register& target, Located<NNPtr<Value>> ptr): target(target), ptr(ptr) {
    ASSERT(dynamic_cast<PointerType const *>(&ptr.value->type()))
}
void IR::Instrs::DerefPtr::accept(InstructionVisitor &v) const { v.visit(*this); }

IR::Instrs::PtrArith::PtrArith(IR::Register& target, Located<NNPtr<Value>> ptr, Located<NNPtr<Value>> offset): target(target), ptr(ptr), offset(offset) {
    ASSERT(dynamic_cast<PointerType const *>(&ptr.value->type()))
    ASSERT(dynamic_cast<IntType const *>(&offset.value->type()) || dynamic_cast<GenericIntType const *>(&offset.value->type()))
}
void IR::Instrs::PtrArith::accept(InstructionVisitor &v) const { v.visit(*this); }

IR::Instrs::Return::Return(Located<NNPtr<Value>> value): value(value) {
}
void IR::Instrs::Return::accept(BrVisitor &v) const { v.visit(*this); }

IR::Instrs::GotoBr::GotoBr(NNPtr<Block> to): to(to) {
}
void IR::Instrs::GotoBr::accept(BrVisitor &v) const { v.visit(*this); }

IR::Instrs::CondBr::CondBr(Located<NNPtr<Value>> v, NNPtr<Block> true_b, NNPtr<Block> false_b): v(v), true_b(true_b), false_b(false_b) {
    ASSERT(dynamic_cast<BoolType const *>(&v.value->type()))
}
void IR::Instrs::CondBr::accept(BrVisitor &v) const { v.visit(*this); }
// INSTR CPP END
