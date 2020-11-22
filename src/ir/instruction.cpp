#include "ir/instruction.h"
#include "ir/value.h"
#include <ostream>

Instrs::Store::Store(Register *target, Value *value): target(target), value(value) {}
void Instrs::Store::stringify(std::ostream &os) const
{
    os << "store " << target->stringify() << " " << value->stringify() << std::endl;
}

Instrs::Or::Or(Register *target, Value *lhs, Value *rhs): target(target), lhs(lhs), rhs(rhs) {}
void Instrs::Or::stringify(std::ostream &os) const
{
    os << "or " << target->stringify() << " " << lhs->stringify() << " " << rhs->stringify() << std::endl;
}
Instrs::And::And(Register *target, Value *lhs, Value *rhs): target(target), lhs(lhs), rhs(rhs) {}
void Instrs::And::stringify(std::ostream &os) const
{
    os << "and " << target->stringify() << " " << lhs->stringify() << " " << rhs->stringify() << std::endl;
}
Instrs::IntCmpNE::IntCmpNE(Register *target, Value *lhs, Value *rhs): target(target), lhs(lhs), rhs(rhs) {}
void Instrs::IntCmpNE::stringify(std::ostream &os) const
{
    os << "intcmpne " << target->stringify() << " " << lhs->stringify() << " " << rhs->stringify() << std::endl;
}
Instrs::IntCmpEQ::IntCmpEQ(Register *target, Value *lhs, Value *rhs): target(target), lhs(lhs), rhs(rhs) {}
void Instrs::IntCmpEQ::stringify(std::ostream &os) const
{
    os << "intcmpeq " << target->stringify() << " " << lhs->stringify() << " " << rhs->stringify() << std::endl;
}
Instrs::IntCmpULT::IntCmpULT(Register *target, Value *lhs, Value *rhs): target(target), lhs(lhs), rhs(rhs) {}
void Instrs::IntCmpULT::stringify(std::ostream &os) const
{
    os << "intcmpult " << target->stringify() << " " << lhs->stringify() << " " << rhs->stringify() << std::endl;
}
Instrs::IntCmpUGT::IntCmpUGT(Register *target, Value *lhs, Value *rhs): target(target), lhs(lhs), rhs(rhs) {}
void Instrs::IntCmpUGT::stringify(std::ostream &os) const
{
    os << "intcmpugt " << target->stringify() << " " << lhs->stringify() << " " << rhs->stringify() << std::endl;
}
Instrs::IntCmpULE::IntCmpULE(Register *target, Value *lhs, Value *rhs): target(target), lhs(lhs), rhs(rhs) {}
void Instrs::IntCmpULE::stringify(std::ostream &os) const
{
    os << "intcmpule " << target->stringify() << " " << lhs->stringify() << " " << rhs->stringify() << std::endl;
}
Instrs::IntCmpUGE::IntCmpUGE(Register *target, Value *lhs, Value *rhs): target(target), lhs(lhs), rhs(rhs) {}
void Instrs::IntCmpUGE::stringify(std::ostream &os) const
{
    os << "intcmpuge " << target->stringify() << " " << lhs->stringify() << " " << rhs->stringify() << std::endl;
}
Instrs::FloatCmpNE::FloatCmpNE(Register *target, Value *lhs, Value *rhs): target(target), lhs(lhs), rhs(rhs) {}
void Instrs::FloatCmpNE::stringify(std::ostream &os) const
{
    os << "floatcmpne " << target->stringify() << " " << lhs->stringify() << " " << rhs->stringify() << std::endl;
}
Instrs::FloatCmpEQ::FloatCmpEQ(Register *target, Value *lhs, Value *rhs): target(target), lhs(lhs), rhs(rhs) {}
void Instrs::FloatCmpEQ::stringify(std::ostream &os) const
{
    os << "floatcmpeq " << target->stringify() << " " << lhs->stringify() << " " << rhs->stringify() << std::endl;
}
Instrs::FloatCmpULT::FloatCmpULT(Register *target, Value *lhs, Value *rhs): target(target), lhs(lhs), rhs(rhs) {}
void Instrs::FloatCmpULT::stringify(std::ostream &os) const
{
    os << "floatcmpult " << target->stringify() << " " << lhs->stringify() << " " << rhs->stringify() << std::endl;
}
Instrs::FloatCmpUGT::FloatCmpUGT(Register *target, Value *lhs, Value *rhs): target(target), lhs(lhs), rhs(rhs) {}
void Instrs::FloatCmpUGT::stringify(std::ostream &os) const
{
    os << "floatcmpugt " << target->stringify() << " " << lhs->stringify() << " " << rhs->stringify() << std::endl;
}
Instrs::FloatCmpULE::FloatCmpULE(Register *target, Value *lhs, Value *rhs): target(target), lhs(lhs), rhs(rhs) {}
void Instrs::FloatCmpULE::stringify(std::ostream &os) const
{
    os << "floatcmpule " << target->stringify() << " " << lhs->stringify() << " " << rhs->stringify() << std::endl;
}
Instrs::FloatCmpUGE::FloatCmpUGE(Register *target, Value *lhs, Value *rhs): target(target), lhs(lhs), rhs(rhs) {}
void Instrs::FloatCmpUGE::stringify(std::ostream &os) const
{
    os << "floatcmpuge " << target->stringify() << " " << lhs->stringify() << " " << rhs->stringify() << std::endl;
}
Instrs::BitXor::BitXor(Register *target, Value *lhs, Value *rhs): target(target), lhs(lhs), rhs(rhs) {}
void Instrs::BitXor::stringify(std::ostream &os) const
{
    os << "bitxor " << target->stringify() << " " << lhs->stringify() << " " << rhs->stringify() << std::endl;
}
Instrs::BitOr::BitOr(Register *target, Value *lhs, Value *rhs): target(target), lhs(lhs), rhs(rhs) {}
void Instrs::BitOr::stringify(std::ostream &os) const
{
    os << "bitor " << target->stringify() << " " << lhs->stringify() << " " << rhs->stringify() << std::endl;
}
Instrs::BitAnd::BitAnd(Register *target, Value *lhs, Value *rhs): target(target), lhs(lhs), rhs(rhs) {}
void Instrs::BitAnd::stringify(std::ostream &os) const
{
    os << "bitand " << target->stringify() << " " << lhs->stringify() << " " << rhs->stringify() << std::endl;
}
Instrs::ShiftR::ShiftR(Register *target, Value *lhs, Value *rhs): target(target), lhs(lhs), rhs(rhs) {}
void Instrs::ShiftR::stringify(std::ostream &os) const
{
    os << "shiftr " << target->stringify() << " " << lhs->stringify() << " " << rhs->stringify() << std::endl;
}
Instrs::ShiftL::ShiftL(Register *target, Value *lhs, Value *rhs): target(target), lhs(lhs), rhs(rhs) {}
void Instrs::ShiftL::stringify(std::ostream &os) const
{
    os << "shiftl " << target->stringify() << " " << lhs->stringify() << " " << rhs->stringify() << std::endl;
}
Instrs::Add::Add(Register *target, Value *lhs, Value *rhs): target(target), lhs(lhs), rhs(rhs) {}
void Instrs::Add::stringify(std::ostream &os) const
{
    os << "add " << target->stringify() << " " << lhs->stringify() << " " << rhs->stringify() << std::endl;
}
Instrs::Sub::Sub(Register *target, Value *lhs, Value *rhs): target(target), lhs(lhs), rhs(rhs) {}
void Instrs::Sub::stringify(std::ostream &os) const
{
    os << "sub " << target->stringify() << " " << lhs->stringify() << " " << rhs->stringify() << std::endl;
}
Instrs::Mult::Mult(Register *target, Value *lhs, Value *rhs): target(target), lhs(lhs), rhs(rhs) {}
void Instrs::Mult::stringify(std::ostream &os) const
{
    os << "mult " << target->stringify() << " " << lhs->stringify() << " " << rhs->stringify() << std::endl;
}
Instrs::Div::Div(Register *target, Value *lhs, Value *rhs): target(target), lhs(lhs), rhs(rhs) {}
void Instrs::Div::stringify(std::ostream &os) const
{
    os << "div " << target->stringify() << " " << lhs->stringify() << " " << rhs->stringify() << std::endl;
}
Instrs::Mod::Mod(Register *target, Value *lhs, Value *rhs): target(target), lhs(lhs), rhs(rhs) {}
void Instrs::Mod::stringify(std::ostream &os) const
{
    os << "mod " << target->stringify() << " " << lhs->stringify() << " " << rhs->stringify() << std::endl;
}
Instrs::BitNot::BitNot(Register *target, Value *op): target(target), op(op) {}
void Instrs::BitNot::stringify(std::ostream &os) const
{
    os << "bitnot " << target->stringify() << " " << op->stringify() << std::endl;
}
Instrs::Neg::Neg(Register *target, Value *op): target(target), op(op) {}
void Instrs::Neg::stringify(std::ostream &os) const
{
    os << "neg " << target->stringify() << " " << op->stringify() << std::endl;
}
Instrs::Trunc::Trunc(Register *target, Value *op, Type *newt): target(target), op(op), newt(newt) {}
void Instrs::Trunc::stringify(std::ostream &os) const
{
    os << "trunc " << target->stringify() << " " << op->stringify() << " to " << newt->stringify() << std::endl;
}
Instrs::ZeroExt::ZeroExt(Register *target, Value *op, Type *newt): target(target), op(op), newt(newt) {}
void Instrs::ZeroExt::stringify(std::ostream &os) const
{
    os << "zeroext " << target->stringify() << " " << op->stringify() << " to " << newt->stringify() << std::endl;
}
Instrs::SignExt::SignExt(Register *target, Value *op, Type *newt): target(target), op(op), newt(newt) {}
void Instrs::SignExt::stringify(std::ostream &os) const
{
    os << "signext " << target->stringify() << " " << op->stringify() << " to " << newt->stringify() << std::endl;
}
Instrs::FloatTrunc::FloatTrunc(Register *target, Value *op, Type *newt): target(target), op(op), newt(newt) {}
void Instrs::FloatTrunc::stringify(std::ostream &os) const
{
    os << "floattrunc " << target->stringify() << " " << op->stringify() << " to " << newt->stringify() << std::endl;
}
Instrs::FloatExt::FloatExt(Register *target, Value *op, Type *newt): target(target), op(op), newt(newt) {}
void Instrs::FloatExt::stringify(std::ostream &os) const
{
    os << "floatext " << target->stringify() << " " << op->stringify() << " to " << newt->stringify() << std::endl;
}
Instrs::SIntToFloat::SIntToFloat(Register *target, Value *op, Type *newt): target(target), op(op), newt(newt) {}
void Instrs::SIntToFloat::stringify(std::ostream &os) const
{
    os << "sinttofloat " << target->stringify() << " " << op->stringify() << " to " << newt->stringify() << std::endl;
}
Instrs::UIntToFloat::UIntToFloat(Register *target, Value *op, Type *newt): target(target), op(op), newt(newt) {}
void Instrs::UIntToFloat::stringify(std::ostream &os) const
{
    os << "uinttofloat " << target->stringify() << " " << op->stringify() << " to " << newt->stringify() << std::endl;
}
Instrs::FloatToSInt::FloatToSInt(Register *target, Value *op, Type *newt): target(target), op(op), newt(newt) {}
void Instrs::FloatToSInt::stringify(std::ostream &os) const
{
    os << "floattosint " << target->stringify() << " " << op->stringify() << " to " << newt->stringify() << std::endl;
}
Instrs::FloatToUInt::FloatToUInt(Register *target, Value *op, Type *newt): target(target), op(op), newt(newt) {}
void Instrs::FloatToUInt::stringify(std::ostream &os) const
{
    os << "floattouint " << target->stringify() << " " << op->stringify() << " to " << newt->stringify() << std::endl;
}
Instrs::Return::Return(Value *value): value(value) {}
void Instrs::Return::stringify(std::ostream &os) const
{
    if (value)
        os << "ret " << value->stringify() << std::endl;
    else
        os << "ret void" << std::endl;
}
Instrs::Call::Call(Register *reg, Function *f, std::vector<Value*> args): reg(reg), f(f), args(args) {}
void Instrs::Call::stringify(std::ostream &os) const
{
    os << "call " << f->name << " ";
    for (Value const *v : args)
        os << v->stringify() << " ";

    if (reg)
        os << "out to " << reg->stringify() << std::endl;
    else
        os << std::endl;
}
