#include "ir/instruction.h"
#include "ir/value.h"
#include "ir/block.h"
#include "lower/visitor.h"
#include <ostream>

Instrs::GotoBr::GotoBr(Block *b): b(b) {}
void Instrs::GotoBr::stringify(std::ostream &os) const
{
    os << "goto " << b->name << std::endl;
}
void Instrs::GotoBr::cfgDot(std::ostream &os) const
{
    os << "branch" << this << " -> block" << b << std::endl;
}

Instrs::CondBr::CondBr(Value *v, Block *trueb, Block *falseb): v(v), trueb(trueb), falseb(falseb) {}
void Instrs::CondBr::stringify(std::ostream &os) const
{
    os << "condbr " << v->stringify() << " " << trueb->name << " " << falseb->name << std::endl;
}
void Instrs::CondBr::cfgDot(std::ostream &os) const
{
    os << "branch" << this << " -> block" << trueb << std::endl;
    os << "branch" << this << " -> block" << falseb << std::endl;
}

#define ACCEPTMETHOD(cl)                              \
    void Instrs::cl::accept(BrVisitor *v)             \
    {                                                 \
        v->visit##cl(this);                           \
    }
ACCEPTMETHOD(GotoBr)
ACCEPTMETHOD(CondBr)
