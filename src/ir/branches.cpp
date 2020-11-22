#include "ir/instruction.h"
#include "ir/value.h"
#include "ir/block.h"
#include <ostream>

Instrs::GotoBr::GotoBr(Block *b): b(b) {}
void Instrs::GotoBr::stringify(std::ostream &os) const
{
    os << "goto " << b->name << std::endl;
}
