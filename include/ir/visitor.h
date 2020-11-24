#pragma once

#include "ir/instruction.h"

namespace IR
{
    class InstructionVisitor
    {
    public:
        virtual ~InstructionVisitor() {}

#define VISITMETHOD(cl) virtual void visit##cl(Instrs::cl *c) = 0;
        VISITMETHOD(Store)
        VISITMETHOD(Or)
        VISITMETHOD(And)
        VISITMETHOD(CmpNE)
        VISITMETHOD(CmpEQ)
        VISITMETHOD(CmpLT)
        VISITMETHOD(CmpGT)
        VISITMETHOD(CmpLE)
        VISITMETHOD(CmpGE)
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
        VISITMETHOD(Ext)
        VISITMETHOD(IntToFloat)
        VISITMETHOD(FloatToInt)
        VISITMETHOD(Return)
        VISITMETHOD(Call)
    };

    class BrVisitor
    {
    public:
        virtual ~BrVisitor() {}
        VISITMETHOD(GotoBr)
        VISITMETHOD(CondBr)
    };
#undef VISITMETHOD
}
