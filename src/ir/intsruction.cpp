#include "ir/instruction.h"
#include <iostream>

using namespace Instrs;

Load::Load(Register *target, Register *value): target(target), value(value) {}
void Load::print()
{
    std::cout << "load " << target->stringify() << " " << value->stringify() << std::endl;
}

LoadConst::LoadConst(Register *target, int value): target(target), value(value) {}
void LoadConst::print()
{
    std::cout << "loadconst" << target->stringify() << " " << value << std::endl;
}

