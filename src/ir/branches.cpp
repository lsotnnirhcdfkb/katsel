#include "ir/instruction.h"
#include "ir/value.h"
#include "ir/block.h"
#include <ostream>

Instrs::GotoBr::GotoBr(Block *b): b(b) {}
void Instrs::GotoBr::stringify(std::ostream &os) const
{
    os << "goto " << b->name << std::endl;
}

Instrs::CondBr::CondBr(Value *v, Block *b): v(v), b(b) {}
void Instrs::CondBr::stringify(std::ostream &os) const
{
    os << "condbr " << v->stringify() << " " << b->name << std::endl;
}
