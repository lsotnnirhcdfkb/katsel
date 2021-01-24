#include "ir/instruction.h"
#include "ir/value.h"
#include "ir/type.h"
#include "ir/visitor.h"
#include "utils/assert.h"
#include "utils/format.h"
#include "codegen/context.h"

// INSTR CPP START
// The following code was autogenerated - see the utils/ directory
IR::Instrs::Store::Store(ASTValue target, ASTValue value, bool init): target(target), value(value), init(init) {
    ASSERT(dynamic_cast<PointerType*>(target.type().as_raw()))
    ASSERT(static_cast<PointerType*>(target.type().as_raw())->ty == value.type())
    ASSERT(init || static_cast<PointerType*>(target.type().as_raw())->mut)
}
void IR::Instrs::Store::accept(InstructionVisitor &v) { v.visit_store(*this); }
NNPtr<IR::Type> IR::Instrs::Store::type() const { return target.type()->context.get_void_type(); }

IR::Instrs::Phi::Phi(std::vector<std::pair<NNPtr<Block>,ASTValue>> prevs): prevs(prevs) {
    ASSERT(prevs.size() > 0)
}
void IR::Instrs::Phi::accept(InstructionVisitor &v) { v.visit_phi(*this); }
NNPtr<IR::Type> IR::Instrs::Phi::type() const { return prevs[0].second.type(); }

IR::Instrs::Register::Register(NNPtr<ASTNS::AST> _def_ast, NNPtr<Type> ty, bool mut): _def_ast(_def_ast), ty(ty), mut(mut) {
}
void IR::Instrs::Register::accept(InstructionVisitor &v) { v.visit_register(*this); }
NNPtr<IR::Type> IR::Instrs::Register::type() const { return ty->context.get_pointer_type(mut, ty); }
NNPtr<ASTNS::AST> IR::Instrs::Register::def_ast() const { return _def_ast; }

IR::Instrs::Or::Or(ASTValue lhs, ASTValue rhs): lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<BoolType*>(lhs.type().as_raw()))
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::Or::accept(InstructionVisitor &v) { v.visit_or(*this); }
NNPtr<IR::Type> IR::Instrs::Or::type() const { return lhs.type()->context.get_bool_type(); }

IR::Instrs::And::And(ASTValue lhs, ASTValue rhs): lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<BoolType*>(lhs.type().as_raw()))
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::And::accept(InstructionVisitor &v) { v.visit_and(*this); }
NNPtr<IR::Type> IR::Instrs::And::type() const { return lhs.type()->context.get_bool_type(); }

IR::Instrs::Not::Not(ASTValue op): op(op) {
}
void IR::Instrs::Not::accept(InstructionVisitor &v) { v.visit_not(*this); }
NNPtr<IR::Type> IR::Instrs::Not::type() const { return op.type()->context.get_bool_type(); }

IR::Instrs::ICmpNE::ICmpNE(ASTValue lhs, ASTValue rhs): lhs(lhs), rhs(rhs) {
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::ICmpNE::accept(InstructionVisitor &v) { v.visit_icmp_ne(*this); }
NNPtr<IR::Type> IR::Instrs::ICmpNE::type() const { return lhs.type()->context.get_bool_type(); }

IR::Instrs::ICmpEQ::ICmpEQ(ASTValue lhs, ASTValue rhs): lhs(lhs), rhs(rhs) {
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::ICmpEQ::accept(InstructionVisitor &v) { v.visit_icmp_eq(*this); }
NNPtr<IR::Type> IR::Instrs::ICmpEQ::type() const { return lhs.type()->context.get_bool_type(); }

IR::Instrs::ICmpLT::ICmpLT(ASTValue lhs, ASTValue rhs): lhs(lhs), rhs(rhs) {
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::ICmpLT::accept(InstructionVisitor &v) { v.visit_icmp_lt(*this); }
NNPtr<IR::Type> IR::Instrs::ICmpLT::type() const { return lhs.type()->context.get_bool_type(); }

IR::Instrs::ICmpGT::ICmpGT(ASTValue lhs, ASTValue rhs): lhs(lhs), rhs(rhs) {
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::ICmpGT::accept(InstructionVisitor &v) { v.visit_icmp_gt(*this); }
NNPtr<IR::Type> IR::Instrs::ICmpGT::type() const { return lhs.type()->context.get_bool_type(); }

IR::Instrs::ICmpLE::ICmpLE(ASTValue lhs, ASTValue rhs): lhs(lhs), rhs(rhs) {
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::ICmpLE::accept(InstructionVisitor &v) { v.visit_icmp_le(*this); }
NNPtr<IR::Type> IR::Instrs::ICmpLE::type() const { return lhs.type()->context.get_bool_type(); }

IR::Instrs::ICmpGE::ICmpGE(ASTValue lhs, ASTValue rhs): lhs(lhs), rhs(rhs) {
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::ICmpGE::accept(InstructionVisitor &v) { v.visit_icmp_ge(*this); }
NNPtr<IR::Type> IR::Instrs::ICmpGE::type() const { return lhs.type()->context.get_bool_type(); }

IR::Instrs::IAdd::IAdd(ASTValue lhs, ASTValue rhs): lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<IntType*>(lhs.type().as_raw()) || dynamic_cast<GenericIntType*>(lhs.type().as_raw()))
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::IAdd::accept(InstructionVisitor &v) { v.visit_iadd(*this); }
NNPtr<IR::Type> IR::Instrs::IAdd::type() const { return lhs.type(); }

IR::Instrs::ISub::ISub(ASTValue lhs, ASTValue rhs): lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<IntType*>(lhs.type().as_raw()) || dynamic_cast<GenericIntType*>(lhs.type().as_raw()))
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::ISub::accept(InstructionVisitor &v) { v.visit_isub(*this); }
NNPtr<IR::Type> IR::Instrs::ISub::type() const { return lhs.type(); }

IR::Instrs::IMult::IMult(ASTValue lhs, ASTValue rhs): lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<IntType*>(lhs.type().as_raw()) || dynamic_cast<GenericIntType*>(lhs.type().as_raw()))
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::IMult::accept(InstructionVisitor &v) { v.visit_imult(*this); }
NNPtr<IR::Type> IR::Instrs::IMult::type() const { return lhs.type(); }

IR::Instrs::IDiv::IDiv(ASTValue lhs, ASTValue rhs): lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<IntType*>(lhs.type().as_raw()) || dynamic_cast<GenericIntType*>(lhs.type().as_raw()))
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::IDiv::accept(InstructionVisitor &v) { v.visit_idiv(*this); }
NNPtr<IR::Type> IR::Instrs::IDiv::type() const { return lhs.type(); }

IR::Instrs::IMod::IMod(ASTValue lhs, ASTValue rhs): lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<IntType*>(lhs.type().as_raw()) || dynamic_cast<GenericIntType*>(lhs.type().as_raw()))
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::IMod::accept(InstructionVisitor &v) { v.visit_imod(*this); }
NNPtr<IR::Type> IR::Instrs::IMod::type() const { return lhs.type(); }

IR::Instrs::INeg::INeg(ASTValue op): op(op) {
    ASSERT(dynamic_cast<IntType*>(op.type().as_raw()) || dynamic_cast<GenericIntType*>(op.type().as_raw()))
}
void IR::Instrs::INeg::accept(InstructionVisitor &v) { v.visit_ineg(*this); }
NNPtr<IR::Type> IR::Instrs::INeg::type() const { return op.type(); }

IR::Instrs::FCmpNE::FCmpNE(ASTValue lhs, ASTValue rhs): lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<FloatType*>(lhs.type().as_raw()) || dynamic_cast<GenericFloatType*>(lhs.type().as_raw()))
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::FCmpNE::accept(InstructionVisitor &v) { v.visit_fcmp_ne(*this); }
NNPtr<IR::Type> IR::Instrs::FCmpNE::type() const { return lhs.type()->context.get_bool_type(); }

IR::Instrs::FCmpEQ::FCmpEQ(ASTValue lhs, ASTValue rhs): lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<FloatType*>(lhs.type().as_raw()) || dynamic_cast<GenericFloatType*>(lhs.type().as_raw()))
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::FCmpEQ::accept(InstructionVisitor &v) { v.visit_fcmp_eq(*this); }
NNPtr<IR::Type> IR::Instrs::FCmpEQ::type() const { return lhs.type()->context.get_bool_type(); }

IR::Instrs::FCmpLT::FCmpLT(ASTValue lhs, ASTValue rhs): lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<FloatType*>(lhs.type().as_raw()) || dynamic_cast<GenericFloatType*>(lhs.type().as_raw()))
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::FCmpLT::accept(InstructionVisitor &v) { v.visit_fcmp_lt(*this); }
NNPtr<IR::Type> IR::Instrs::FCmpLT::type() const { return lhs.type()->context.get_bool_type(); }

IR::Instrs::FCmpGT::FCmpGT(ASTValue lhs, ASTValue rhs): lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<FloatType*>(lhs.type().as_raw()) || dynamic_cast<GenericFloatType*>(lhs.type().as_raw()))
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::FCmpGT::accept(InstructionVisitor &v) { v.visit_fcmp_gt(*this); }
NNPtr<IR::Type> IR::Instrs::FCmpGT::type() const { return lhs.type()->context.get_bool_type(); }

IR::Instrs::FCmpLE::FCmpLE(ASTValue lhs, ASTValue rhs): lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<FloatType*>(lhs.type().as_raw()) || dynamic_cast<GenericFloatType*>(lhs.type().as_raw()))
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::FCmpLE::accept(InstructionVisitor &v) { v.visit_fcmp_le(*this); }
NNPtr<IR::Type> IR::Instrs::FCmpLE::type() const { return lhs.type()->context.get_bool_type(); }

IR::Instrs::FCmpGE::FCmpGE(ASTValue lhs, ASTValue rhs): lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<FloatType*>(lhs.type().as_raw()) || dynamic_cast<GenericFloatType*>(lhs.type().as_raw()))
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::FCmpGE::accept(InstructionVisitor &v) { v.visit_fcmp_ge(*this); }
NNPtr<IR::Type> IR::Instrs::FCmpGE::type() const { return lhs.type()->context.get_bool_type(); }

IR::Instrs::FAdd::FAdd(ASTValue lhs, ASTValue rhs): lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<FloatType*>(lhs.type().as_raw()) || dynamic_cast<GenericFloatType*>(lhs.type().as_raw()))
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::FAdd::accept(InstructionVisitor &v) { v.visit_fadd(*this); }
NNPtr<IR::Type> IR::Instrs::FAdd::type() const { return lhs.type(); }

IR::Instrs::FSub::FSub(ASTValue lhs, ASTValue rhs): lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<FloatType*>(lhs.type().as_raw()) || dynamic_cast<GenericFloatType*>(lhs.type().as_raw()))
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::FSub::accept(InstructionVisitor &v) { v.visit_fsub(*this); }
NNPtr<IR::Type> IR::Instrs::FSub::type() const { return lhs.type(); }

IR::Instrs::FMult::FMult(ASTValue lhs, ASTValue rhs): lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<FloatType*>(lhs.type().as_raw()) || dynamic_cast<GenericFloatType*>(lhs.type().as_raw()))
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::FMult::accept(InstructionVisitor &v) { v.visit_fmult(*this); }
NNPtr<IR::Type> IR::Instrs::FMult::type() const { return lhs.type(); }

IR::Instrs::FDiv::FDiv(ASTValue lhs, ASTValue rhs): lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<FloatType*>(lhs.type().as_raw()) || dynamic_cast<GenericFloatType*>(lhs.type().as_raw()))
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::FDiv::accept(InstructionVisitor &v) { v.visit_fdiv(*this); }
NNPtr<IR::Type> IR::Instrs::FDiv::type() const { return lhs.type(); }

IR::Instrs::FMod::FMod(ASTValue lhs, ASTValue rhs): lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<FloatType*>(lhs.type().as_raw()) || dynamic_cast<GenericFloatType*>(lhs.type().as_raw()))
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::FMod::accept(InstructionVisitor &v) { v.visit_fmod(*this); }
NNPtr<IR::Type> IR::Instrs::FMod::type() const { return lhs.type(); }

IR::Instrs::FNeg::FNeg(ASTValue op): op(op) {
    ASSERT(dynamic_cast<FloatType*>(op.type().as_raw()) || dynamic_cast<GenericFloatType*>(op.type().as_raw()))
}
void IR::Instrs::FNeg::accept(InstructionVisitor &v) { v.visit_fneg(*this); }
NNPtr<IR::Type> IR::Instrs::FNeg::type() const { return op.type(); }

IR::Instrs::BitXor::BitXor(ASTValue lhs, ASTValue rhs): lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<IntType*>(lhs.type().as_raw()) || dynamic_cast<GenericIntType*>(lhs.type().as_raw()))
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::BitXor::accept(InstructionVisitor &v) { v.visit_bit_xor(*this); }
NNPtr<IR::Type> IR::Instrs::BitXor::type() const { return lhs.type(); }

IR::Instrs::BitOr::BitOr(ASTValue lhs, ASTValue rhs): lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<IntType*>(lhs.type().as_raw()) || dynamic_cast<GenericIntType*>(lhs.type().as_raw()))
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::BitOr::accept(InstructionVisitor &v) { v.visit_bit_or(*this); }
NNPtr<IR::Type> IR::Instrs::BitOr::type() const { return lhs.type(); }

IR::Instrs::BitAnd::BitAnd(ASTValue lhs, ASTValue rhs): lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<IntType*>(lhs.type().as_raw()) || dynamic_cast<GenericIntType*>(lhs.type().as_raw()))
    ASSERT(lhs.type() == rhs.type())
}
void IR::Instrs::BitAnd::accept(InstructionVisitor &v) { v.visit_bit_and(*this); }
NNPtr<IR::Type> IR::Instrs::BitAnd::type() const { return lhs.type(); }

IR::Instrs::BitNot::BitNot(ASTValue op): op(op) {
    ASSERT(dynamic_cast<IntType*>(op.type().as_raw()) || dynamic_cast<GenericIntType*>(op.type().as_raw()))
}
void IR::Instrs::BitNot::accept(InstructionVisitor &v) { v.visit_bit_not(*this); }
NNPtr<IR::Type> IR::Instrs::BitNot::type() const { return op.type(); }

IR::Instrs::ShiftR::ShiftR(ASTValue lhs, ASTValue rhs): lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<IntType*>(lhs.type().as_raw()) || dynamic_cast<GenericIntType*>(lhs.type().as_raw()))
    ASSERT(dynamic_cast<IntType*>(lhs.type().as_raw()) || dynamic_cast<GenericIntType*>(lhs.type().as_raw()))
}
void IR::Instrs::ShiftR::accept(InstructionVisitor &v) { v.visit_shift_r(*this); }
NNPtr<IR::Type> IR::Instrs::ShiftR::type() const { return lhs.type(); }

IR::Instrs::ShiftL::ShiftL(ASTValue lhs, ASTValue rhs): lhs(lhs), rhs(rhs) {
    ASSERT(dynamic_cast<IntType*>(lhs.type().as_raw()) || dynamic_cast<GenericIntType*>(lhs.type().as_raw()))
    ASSERT(dynamic_cast<IntType*>(lhs.type().as_raw()) || dynamic_cast<GenericIntType*>(lhs.type().as_raw()))
}
void IR::Instrs::ShiftL::accept(InstructionVisitor &v) { v.visit_shift_l(*this); }
NNPtr<IR::Type> IR::Instrs::ShiftL::type() const { return lhs.type(); }

IR::Instrs::NoOpCast::NoOpCast(ASTValue op, NNPtr<Type> newt): op(op), newt(newt) {
}
void IR::Instrs::NoOpCast::accept(InstructionVisitor &v) { v.visit_no_op_cast(*this); }
NNPtr<IR::Type> IR::Instrs::NoOpCast::type() const { return newt; }

IR::Instrs::IntToInt::IntToInt(ASTValue op, NNPtr<IntType> newt): op(op), newt(newt) {
    ASSERT(dynamic_cast<IntType*>(op.type().as_raw()) || dynamic_cast<GenericIntType*>(op.type().as_raw()))
}
void IR::Instrs::IntToInt::accept(InstructionVisitor &v) { v.visit_int_to_int(*this); }
NNPtr<IR::Type> IR::Instrs::IntToInt::type() const { return newt; }

IR::Instrs::IntToFloat::IntToFloat(ASTValue op, NNPtr<FloatType> newt): op(op), newt(newt) {
    ASSERT(dynamic_cast<IntType*>(op.type().as_raw()) || dynamic_cast<GenericIntType*>(op.type().as_raw()))
}
void IR::Instrs::IntToFloat::accept(InstructionVisitor &v) { v.visit_int_to_float(*this); }
NNPtr<IR::Type> IR::Instrs::IntToFloat::type() const { return newt; }

IR::Instrs::FloatToFloat::FloatToFloat(ASTValue op, NNPtr<FloatType> newt): op(op), newt(newt) {
    ASSERT(dynamic_cast<FloatType*>(op.type().as_raw()) || dynamic_cast<GenericFloatType*>(op.type().as_raw()))
}
void IR::Instrs::FloatToFloat::accept(InstructionVisitor &v) { v.visit_float_to_float(*this); }
NNPtr<IR::Type> IR::Instrs::FloatToFloat::type() const { return newt; }

IR::Instrs::FloatToInt::FloatToInt(ASTValue op, NNPtr<IntType> newt): op(op), newt(newt) {
    ASSERT(dynamic_cast<FloatType*>(op.type().as_raw()) || dynamic_cast<GenericFloatType*>(op.type().as_raw()))
}
void IR::Instrs::FloatToInt::accept(InstructionVisitor &v) { v.visit_float_to_int(*this); }
NNPtr<IR::Type> IR::Instrs::FloatToInt::type() const { return newt; }

IR::Instrs::Call::Call(NNPtr<Function> f, std::vector<ASTValue> args): f(f), args(args) {
    ASSERT(args.size() == f->ty->paramtys.size())
}
void IR::Instrs::Call::accept(InstructionVisitor &v) { v.visit_call(*this); }
NNPtr<IR::Type> IR::Instrs::Call::type() const { return f->ty->ret; }

IR::Instrs::Addrof::Addrof(NNPtr<DerefPtr> deref, bool mut): deref(deref), mut(mut) {
    ASSERT(static_cast<int>(mut) <= static_cast<int>(static_cast<PointerType*>(deref->ptr.type().as_raw())->mut))
}
void IR::Instrs::Addrof::accept(InstructionVisitor &v) { v.visit_addrof(*this); }
NNPtr<IR::Type> IR::Instrs::Addrof::type() const { return deref->type()->context.get_pointer_type(mut, deref->type()); }

IR::Instrs::DerefPtr::DerefPtr(ASTValue ptr): ptr(ptr) {
    ASSERT(dynamic_cast<PointerType*>(ptr.type().as_raw()))
}
void IR::Instrs::DerefPtr::accept(InstructionVisitor &v) { v.visit_deref_ptr(*this); }
NNPtr<IR::Type> IR::Instrs::DerefPtr::type() const { return static_cast<PointerType*>(ptr.type().as_raw())->ty; }

IR::Instrs::PtrArith::PtrArith(ASTValue ptr, ASTValue offset): ptr(ptr), offset(offset) {
    ASSERT(dynamic_cast<PointerType*>(ptr.type().as_raw()))
    ASSERT(dynamic_cast<IntType*>(offset.type().as_raw()) || dynamic_cast<GenericIntType*>(offset.type().as_raw()))
}
void IR::Instrs::PtrArith::accept(InstructionVisitor &v) { v.visit_ptr_arith(*this); }
NNPtr<IR::Type> IR::Instrs::PtrArith::type() const { return ptr.type(); }

IR::Instrs::Return::Return(ASTValue value): value(value) {
}
void IR::Instrs::Return::accept(BrVisitor &v) { v.visit_return(*this); }

IR::Instrs::GotoBr::GotoBr(NNPtr<Block> to): to(to) {
}
void IR::Instrs::GotoBr::accept(BrVisitor &v) { v.visit_goto_br(*this); }

IR::Instrs::CondBr::CondBr(ASTValue v, NNPtr<Block> true_b, NNPtr<Block> false_b): v(v), true_b(true_b), false_b(false_b) {
    ASSERT(dynamic_cast<BoolType*>(v.type().as_raw()))
}
void IR::Instrs::CondBr::accept(BrVisitor &v) { v.visit_cond_br(*this); }

// This code was autogenerated - see the utils/ directory
// INSTR CPP END
