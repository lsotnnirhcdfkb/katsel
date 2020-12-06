#pragma once

#include "ir/visitor.h"
#include "ir/instruction.h"
#include "llvm/Support/raw_ostream.h"

namespace IR
{
    class Printer : public IR::InstructionVisitor, public IR::BrVisitor
    {
    public:
        Printer(llvm::raw_ostream &ostream);
#define VISITMETHOD(cl) void visit##cl(Instrs::cl *i) override;
        VISITMETHOD(Store)
        VISITMETHOD(Phi)
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
        VISITMETHOD(GotoBr)
        VISITMETHOD(CondBr)
#undef VISITMETHOD

    private:
        llvm::raw_ostream &ostream;
    };
}
