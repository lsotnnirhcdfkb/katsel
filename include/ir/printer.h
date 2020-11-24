#pragma once

#include "ir/visitor.h"
#include "ir/instruction.h"
#include <ostream>

namespace IR
{
    class Printer : public IR::InstructionVisitor, public IR::BrVisitor
    {
    public:
        Printer(std::ostream &ostream);
#define VISITMETHOD(cl) void visit##cl(Instrs::cl *i) override;
        VISITMETHOD(Store)
        VISITMETHOD(Or)
        VISITMETHOD(And)
        VISITMETHOD(IntCmpNE)
        VISITMETHOD(IntCmpEQ)
        VISITMETHOD(IntCmpULT)
        VISITMETHOD(IntCmpUGT)
        VISITMETHOD(IntCmpULE)
        VISITMETHOD(IntCmpUGE)
        VISITMETHOD(FloatCmpNE)
        VISITMETHOD(FloatCmpEQ)
        VISITMETHOD(FloatCmpULT)
        VISITMETHOD(FloatCmpUGT)
        VISITMETHOD(FloatCmpULE)
        VISITMETHOD(FloatCmpUGE)
        VISITMETHOD(BitXor)
        VISITMETHOD(BitOr)
        VISITMETHOD(BitAnd)
        VISITMETHOD(BitNot)
        VISITMETHOD(ShiftR)
        VISITMETHOD(ShiftL)
        VISITMETHOD(Add)
        VISITMETHOD(Sub)
        VISITMETHOD(Mult)
        VISITMETHOD(Div)
        VISITMETHOD(Mod)
        VISITMETHOD(Neg)
        VISITMETHOD(Trunc)
        VISITMETHOD(ZeroExt)
        VISITMETHOD(SignExt)
        VISITMETHOD(FloatTrunc)
        VISITMETHOD(FloatExt)
        VISITMETHOD(SIntToFloat)
        VISITMETHOD(UIntToFloat)
        VISITMETHOD(FloatToSInt)
        VISITMETHOD(FloatToUInt)
        VISITMETHOD(Return)
        VISITMETHOD(Call)
        VISITMETHOD(GotoBr)
        VISITMETHOD(CondBr)
#undef VISITMETHOD

    private:
        std::ostream &ostream;
    };
}
