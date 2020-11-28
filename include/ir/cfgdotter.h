#pragma once

#include "ir/instruction.h"
#include "ir/visitor.h"
#include "llvm/Support/raw_ostream.h"

namespace IR
{
    class CFGDotter : public BrVisitor
    {
    public:
        CFGDotter(llvm::raw_ostream &ostream);

#define VISITMETHOD(cl) void visit##cl(Instrs::cl *i) override;
        VISITMETHOD(GotoBr)
        VISITMETHOD(CondBr)
#undef VISITMETHOD
    private:
        llvm::raw_ostream &ostream;
    };
}
