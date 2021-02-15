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
}
void IR::Instrs::Copy::instr_accept(InstructionVisitor &v) const { v.instr_visit(*this); }
IR::Type const &IR::Instrs::Copy::type() const { return val.value->type().context.get_void_type(); }

IR::Instrs::Or::Or(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<BoolType const *>(&lhs.value->type()))
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::Or::instr_accept(InstructionVisitor &v) const { v.instr_visit(*this); }
IR::Type const &IR::Instrs::Or::type() const { return lhs.value->type(); }

IR::Instrs::And::And(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<BoolType const *>(&lhs.value->type()))
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::And::instr_accept(InstructionVisitor &v) const { v.instr_visit(*this); }
IR::Type const &IR::Instrs::And::type() const { return lhs.value->type(); }

IR::Instrs::Not::Not(Located<NNPtr<Value>> op): op(op) {
    ASSERT(dynamic_cast<BoolType const *>(&op.value->type()))
}
void IR::Instrs::Not::instr_accept(InstructionVisitor &v) const { v.instr_visit(*this); }
IR::Type const &IR::Instrs::Not::type() const { return op.value->type().context.get_bool_type(); }

IR::Instrs::ICmpNE::ICmpNE(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): lhs(lhs), rhs(rhs) {
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::ICmpNE::instr_accept(InstructionVisitor &v) const { v.instr_visit(*this); }
IR::Type const &IR::Instrs::ICmpNE::type() const { return lhs.value->type().context.get_bool_type(); }

IR::Instrs::ICmpEQ::ICmpEQ(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): lhs(lhs), rhs(rhs) {
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::ICmpEQ::instr_accept(InstructionVisitor &v) const { v.instr_visit(*this); }
IR::Type const &IR::Instrs::ICmpEQ::type() const { return lhs.value->type().context.get_bool_type(); }

IR::Instrs::ICmpLT::ICmpLT(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): lhs(lhs), rhs(rhs) {
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::ICmpLT::instr_accept(InstructionVisitor &v) const { v.instr_visit(*this); }
IR::Type const &IR::Instrs::ICmpLT::type() const { return lhs.value->type().context.get_bool_type(); }

IR::Instrs::ICmpGT::ICmpGT(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): lhs(lhs), rhs(rhs) {
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::ICmpGT::instr_accept(InstructionVisitor &v) const { v.instr_visit(*this); }
IR::Type const &IR::Instrs::ICmpGT::type() const { return lhs.value->type().context.get_bool_type(); }

IR::Instrs::ICmpLE::ICmpLE(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): lhs(lhs), rhs(rhs) {
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::ICmpLE::instr_accept(InstructionVisitor &v) const { v.instr_visit(*this); }
IR::Type const &IR::Instrs::ICmpLE::type() const { return lhs.value->type().context.get_bool_type(); }

IR::Instrs::ICmpGE::ICmpGE(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): lhs(lhs), rhs(rhs) {
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::ICmpGE::instr_accept(InstructionVisitor &v) const { v.instr_visit(*this); }
IR::Type const &IR::Instrs::ICmpGE::type() const { return lhs.value->type().context.get_bool_type(); }

IR::Instrs::IAdd::IAdd(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<IntType const *>(&lhs.value->type()) || dynamic_cast<GenericIntType const *>(&lhs.value->type()))
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::IAdd::instr_accept(InstructionVisitor &v) const { v.instr_visit(*this); }
IR::Type const &IR::Instrs::IAdd::type() const { return lhs.value->type(); }

IR::Instrs::ISub::ISub(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<IntType const *>(&lhs.value->type()) || dynamic_cast<GenericIntType const *>(&lhs.value->type()))
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::ISub::instr_accept(InstructionVisitor &v) const { v.instr_visit(*this); }
IR::Type const &IR::Instrs::ISub::type() const { return lhs.value->type(); }

IR::Instrs::IMult::IMult(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<IntType const *>(&lhs.value->type()) || dynamic_cast<GenericIntType const *>(&lhs.value->type()))
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::IMult::instr_accept(InstructionVisitor &v) const { v.instr_visit(*this); }
IR::Type const &IR::Instrs::IMult::type() const { return lhs.value->type(); }

IR::Instrs::IDiv::IDiv(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<IntType const *>(&lhs.value->type()) || dynamic_cast<GenericIntType const *>(&lhs.value->type()))
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::IDiv::instr_accept(InstructionVisitor &v) const { v.instr_visit(*this); }
IR::Type const &IR::Instrs::IDiv::type() const { return lhs.value->type(); }

IR::Instrs::IMod::IMod(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<IntType const *>(&lhs.value->type()) || dynamic_cast<GenericIntType const *>(&lhs.value->type()))
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::IMod::instr_accept(InstructionVisitor &v) const { v.instr_visit(*this); }
IR::Type const &IR::Instrs::IMod::type() const { return lhs.value->type(); }

IR::Instrs::INeg::INeg(Located<NNPtr<Value>> op): op(op) {
    ASSERT(dynamic_cast<IntType const *>(&op.value->type()) || dynamic_cast<GenericIntType const *>(&op.value->type()))
}
void IR::Instrs::INeg::instr_accept(InstructionVisitor &v) const { v.instr_visit(*this); }
IR::Type const &IR::Instrs::INeg::type() const { return op.value->type(); }

IR::Instrs::FCmpNE::FCmpNE(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<FloatType const *>(&lhs.value->type()) || dynamic_cast<GenericFloatType const *>(&lhs.value->type()))
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::FCmpNE::instr_accept(InstructionVisitor &v) const { v.instr_visit(*this); }
IR::Type const &IR::Instrs::FCmpNE::type() const { return lhs.value->type().context.get_bool_type(); }

IR::Instrs::FCmpEQ::FCmpEQ(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<FloatType const *>(&lhs.value->type()) || dynamic_cast<GenericFloatType const *>(&lhs.value->type()))
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::FCmpEQ::instr_accept(InstructionVisitor &v) const { v.instr_visit(*this); }
IR::Type const &IR::Instrs::FCmpEQ::type() const { return lhs.value->type().context.get_bool_type(); }

IR::Instrs::FCmpLT::FCmpLT(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<FloatType const *>(&lhs.value->type()) || dynamic_cast<GenericFloatType const *>(&lhs.value->type()))
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::FCmpLT::instr_accept(InstructionVisitor &v) const { v.instr_visit(*this); }
IR::Type const &IR::Instrs::FCmpLT::type() const { return lhs.value->type().context.get_bool_type(); }

IR::Instrs::FCmpGT::FCmpGT(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<FloatType const *>(&lhs.value->type()) || dynamic_cast<GenericFloatType const *>(&lhs.value->type()))
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::FCmpGT::instr_accept(InstructionVisitor &v) const { v.instr_visit(*this); }
IR::Type const &IR::Instrs::FCmpGT::type() const { return lhs.value->type().context.get_bool_type(); }

IR::Instrs::FCmpLE::FCmpLE(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<FloatType const *>(&lhs.value->type()) || dynamic_cast<GenericFloatType const *>(&lhs.value->type()))
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::FCmpLE::instr_accept(InstructionVisitor &v) const { v.instr_visit(*this); }
IR::Type const &IR::Instrs::FCmpLE::type() const { return lhs.value->type().context.get_bool_type(); }

IR::Instrs::FCmpGE::FCmpGE(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<FloatType const *>(&lhs.value->type()) || dynamic_cast<GenericFloatType const *>(&lhs.value->type()))
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::FCmpGE::instr_accept(InstructionVisitor &v) const { v.instr_visit(*this); }
IR::Type const &IR::Instrs::FCmpGE::type() const { return lhs.value->type().context.get_bool_type(); }

IR::Instrs::FAdd::FAdd(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<FloatType const *>(&lhs.value->type()) || dynamic_cast<GenericFloatType const *>(&lhs.value->type()))
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::FAdd::instr_accept(InstructionVisitor &v) const { v.instr_visit(*this); }
IR::Type const &IR::Instrs::FAdd::type() const { return lhs.value->type(); }

IR::Instrs::FSub::FSub(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<FloatType const *>(&lhs.value->type()) || dynamic_cast<GenericFloatType const *>(&lhs.value->type()))
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::FSub::instr_accept(InstructionVisitor &v) const { v.instr_visit(*this); }
IR::Type const &IR::Instrs::FSub::type() const { return lhs.value->type(); }

IR::Instrs::FMult::FMult(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<FloatType const *>(&lhs.value->type()) || dynamic_cast<GenericFloatType const *>(&lhs.value->type()))
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::FMult::instr_accept(InstructionVisitor &v) const { v.instr_visit(*this); }
IR::Type const &IR::Instrs::FMult::type() const { return lhs.value->type(); }

IR::Instrs::FDiv::FDiv(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<FloatType const *>(&lhs.value->type()) || dynamic_cast<GenericFloatType const *>(&lhs.value->type()))
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::FDiv::instr_accept(InstructionVisitor &v) const { v.instr_visit(*this); }
IR::Type const &IR::Instrs::FDiv::type() const { return lhs.value->type(); }

IR::Instrs::FMod::FMod(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<FloatType const *>(&lhs.value->type()) || dynamic_cast<GenericFloatType const *>(&lhs.value->type()))
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::FMod::instr_accept(InstructionVisitor &v) const { v.instr_visit(*this); }
IR::Type const &IR::Instrs::FMod::type() const { return lhs.value->type(); }

IR::Instrs::FNeg::FNeg(Located<NNPtr<Value>> op): op(op) {
    ASSERT(dynamic_cast<FloatType const *>(&op.value->type()) || dynamic_cast<GenericFloatType const *>(&op.value->type()))
}
void IR::Instrs::FNeg::instr_accept(InstructionVisitor &v) const { v.instr_visit(*this); }
IR::Type const &IR::Instrs::FNeg::type() const { return op.value->type(); }

IR::Instrs::BitXor::BitXor(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<IntType const *>(&lhs.value->type()) || dynamic_cast<GenericIntType const *>(&lhs.value->type()))
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::BitXor::instr_accept(InstructionVisitor &v) const { v.instr_visit(*this); }
IR::Type const &IR::Instrs::BitXor::type() const { return lhs.value->type(); }

IR::Instrs::BitOr::BitOr(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<IntType const *>(&lhs.value->type()) || dynamic_cast<GenericIntType const *>(&lhs.value->type()))
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::BitOr::instr_accept(InstructionVisitor &v) const { v.instr_visit(*this); }
IR::Type const &IR::Instrs::BitOr::type() const { return lhs.value->type(); }

IR::Instrs::BitAnd::BitAnd(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<IntType const *>(&lhs.value->type()) || dynamic_cast<GenericIntType const *>(&lhs.value->type()))
    ASSERT(&lhs.value->type() == &rhs.value->type())
}
void IR::Instrs::BitAnd::instr_accept(InstructionVisitor &v) const { v.instr_visit(*this); }
IR::Type const &IR::Instrs::BitAnd::type() const { return lhs.value->type(); }

IR::Instrs::BitNot::BitNot(Located<NNPtr<Value>> op): op(op) {
    ASSERT(dynamic_cast<IntType const *>(&op.value->type()) || dynamic_cast<GenericIntType const *>(&op.value->type()))
}
void IR::Instrs::BitNot::instr_accept(InstructionVisitor &v) const { v.instr_visit(*this); }
IR::Type const &IR::Instrs::BitNot::type() const { return op.value->type(); }

IR::Instrs::ShiftR::ShiftR(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<IntType const *>(&lhs.value->type()) || dynamic_cast<GenericIntType const *>(&lhs.value->type()))
    ASSERT(dynamic_cast<IntType const *>(&lhs.value->type()) || dynamic_cast<GenericIntType const *>(&lhs.value->type()))
}
void IR::Instrs::ShiftR::instr_accept(InstructionVisitor &v) const { v.instr_visit(*this); }
IR::Type const &IR::Instrs::ShiftR::type() const { return lhs.value->type(); }

IR::Instrs::ShiftL::ShiftL(Located<NNPtr<Value>> lhs, Located<NNPtr<Value>> rhs): lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<IntType const *>(&lhs.value->type()) || dynamic_cast<GenericIntType const *>(&lhs.value->type()))
    ASSERT(dynamic_cast<IntType const *>(&lhs.value->type()) || dynamic_cast<GenericIntType const *>(&lhs.value->type()))
}
void IR::Instrs::ShiftL::instr_accept(InstructionVisitor &v) const { v.instr_visit(*this); }
IR::Type const &IR::Instrs::ShiftL::type() const { return lhs.value->type(); }

IR::Instrs::NoOpCast::NoOpCast(Located<NNPtr<Value>> op, NNPtr<Type const> newt): op(op), newt(newt) {
}
void IR::Instrs::NoOpCast::instr_accept(InstructionVisitor &v) const { v.instr_visit(*this); }
IR::Type const &IR::Instrs::NoOpCast::type() const { return *newt; }

IR::Instrs::IntToInt::IntToInt(Located<NNPtr<Value>> op, NNPtr<IntType const> newt): op(op), newt(newt) {
    ASSERT(dynamic_cast<IntType const *>(&op.value->type()) || dynamic_cast<GenericIntType const *>(&op.value->type()))
}
void IR::Instrs::IntToInt::instr_accept(InstructionVisitor &v) const { v.instr_visit(*this); }
IR::Type const &IR::Instrs::IntToInt::type() const { return *newt; }

IR::Instrs::IntToFloat::IntToFloat(Located<NNPtr<Value>> op, NNPtr<FloatType const> newt): op(op), newt(newt) {
    ASSERT(dynamic_cast<IntType const *>(&op.value->type()) || dynamic_cast<GenericIntType const *>(&op.value->type()))
}
void IR::Instrs::IntToFloat::instr_accept(InstructionVisitor &v) const { v.instr_visit(*this); }
IR::Type const &IR::Instrs::IntToFloat::type() const { return *newt; }

IR::Instrs::FloatToFloat::FloatToFloat(Located<NNPtr<Value>> op, NNPtr<FloatType const> newt): op(op), newt(newt) {
    ASSERT(dynamic_cast<FloatType const *>(&op.value->type()) || dynamic_cast<GenericFloatType const *>(&op.value->type()))
}
void IR::Instrs::FloatToFloat::instr_accept(InstructionVisitor &v) const { v.instr_visit(*this); }
IR::Type const &IR::Instrs::FloatToFloat::type() const { return *newt; }

IR::Instrs::FloatToInt::FloatToInt(Located<NNPtr<Value>> op, NNPtr<IntType const> newt): op(op), newt(newt) {
    ASSERT(dynamic_cast<FloatType const *>(&op.value->type()) || dynamic_cast<GenericFloatType const *>(&op.value->type()))
}
void IR::Instrs::FloatToInt::instr_accept(InstructionVisitor &v) const { v.instr_visit(*this); }
IR::Type const &IR::Instrs::FloatToInt::type() const { return *newt; }

IR::Instrs::Call::Call(NNPtr<Function const> f, std::vector<Located<NNPtr<Value>>> args): f(f), args(args) {
    ASSERT(args.size() == f->ty->paramtys.size())
}
void IR::Instrs::Call::instr_accept(InstructionVisitor &v) const { v.instr_visit(*this); }
IR::Type const &IR::Instrs::Call::type() const { return *f->ty->ret; }

IR::Instrs::Addrof::Addrof(IR::Register& reg, bool mut): reg(reg), mut(mut) {
    ASSERT(static_cast<int>(mut) <= static_cast<int>(reg.mut))
}
void IR::Instrs::Addrof::instr_accept(InstructionVisitor &v) const { v.instr_visit(*this); }
IR::Type const &IR::Instrs::Addrof::type() const { return reg.type().context.get_pointer_type(mut, reg.type()); }

IR::Instrs::DerefPtr::DerefPtr(Located<NNPtr<Value>> ptr): ptr(ptr) {
    ASSERT(dynamic_cast<PointerType const *>(&ptr.value->type()))
}
void IR::Instrs::DerefPtr::instr_accept(InstructionVisitor &v) const { v.instr_visit(*this); }
IR::Type const &IR::Instrs::DerefPtr::type() const { return *static_cast<PointerType const *>(&ptr.value->type())->ty; }

IR::Instrs::PtrArith::PtrArith(Located<NNPtr<Value>> ptr, Located<NNPtr<Value>> offset): ptr(ptr), offset(offset) {
    ASSERT(dynamic_cast<PointerType const *>(&ptr.value->type()))
    ASSERT(dynamic_cast<IntType const *>(&offset.value->type()) || dynamic_cast<GenericIntType const *>(&offset.value->type()))
}
void IR::Instrs::PtrArith::instr_accept(InstructionVisitor &v) const { v.instr_visit(*this); }
IR::Type const &IR::Instrs::PtrArith::type() const { return ptr.value->type(); }

IR::Instrs::Return::Return(Located<NNPtr<Value>> value): value(value) {
}
void IR::Instrs::Return::instr_accept(BrVisitor &v) const { v.instr_visit(*this); }

IR::Instrs::GotoBr::GotoBr(NNPtr<Block> to): to(to) {
}
void IR::Instrs::GotoBr::instr_accept(BrVisitor &v) const { v.instr_visit(*this); }

IR::Instrs::CondBr::CondBr(Located<NNPtr<Value>> v, NNPtr<Block> true_b, NNPtr<Block> false_b): v(v), true_b(true_b), false_b(false_b) {
    ASSERT(dynamic_cast<BoolType const *>(&v.value->type()))
}
void IR::Instrs::CondBr::instr_accept(BrVisitor &v) const { v.instr_visit(*this); }
// INSTR CPP END
