#include "ir/unit.h"

Unit::Unit(File const &file): file(file) {}

void Unit::print(std::ostream &ostream) const
{
    ostream << "Unit " << file.filename << std::endl;
    for (Function const &f : functions)
    {
        ostream << f.stringify() << std::endl;
    }
}
