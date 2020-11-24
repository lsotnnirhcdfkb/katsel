#pragma once

#include "ir/instruction.h"
#include "ir/visitor.h"
#include <ostream>

namespace IR
{
    class CFGDotter : public BrVisitor
    {
    public:
        CFGDotter(std::ostream &ostream);

#define VISITMETHOD(cl) void visit##cl(Instrs::cl *i) override;
        VISITMETHOD(GotoBr)
        VISITMETHOD(CondBr)
#undef VISITMETHOD
    private:
        std::ostream &ostream;
    };
}
