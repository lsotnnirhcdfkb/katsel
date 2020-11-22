#include "ir/instruction.h"
#include "ir/value.h"
#include <iostream>

Instrs::Store::Store(Register *target, Value *value): target(target), value(value) {}
void Instrs::Store::stringify(std::ostream &s) const
{
    s << "store " << target->stringify() << " " << value->stringify() << std::endl;
}
