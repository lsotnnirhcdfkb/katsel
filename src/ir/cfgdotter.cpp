#include "ir/cfgdotter.h"
#include "ir/block.h"
#include "ir/value.h"

IR::CFGDotter::CFGDotter(std::ostream &ostream): ostream(ostream) {}

// CFGDOTTER START

// The following code was autogenerated - see the utils/ directory
void IR::CFGDotter::visitGotoBr(IR::Instrs::GotoBr *i)
{
    ostream << "        branch" << i << " -> " << "block" << i->to << " [label=\"to\"]" << std::endl;;
}
void IR::CFGDotter::visitCondBr(IR::Instrs::CondBr *i)
{
    ostream << "        branch" << i << " -> " << "block" << i->trueB << " [label=\"trueB\"]" << std::endl;;
    ostream << "        branch" << i << " -> " << "block" << i->falseB << " [label=\"falseB\"]" << std::endl;;
}
// This code was autogenerated - see the utils/ directory

// CFGDOTTER END
