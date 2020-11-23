#include "ir/instruction.h"

namespace IR
{
    class InstructionVisitor
    {
    public:
#define VISITMETHOD(cl) virtual void visit##cl(Instrs::cl *c) = 0;
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
    };

    class BrVisitor
    {
    public:
        VISITMETHOD(GotoBr)
        VISITMETHOD(CondBr)
    };
#undef VISITMETHOD
}
