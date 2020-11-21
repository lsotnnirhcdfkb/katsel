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

Function* Unit::addFunction(FunctionType *type, std::string name, ASTNS::Function *ast)
{
    functions.emplace_back(type, name, ast);
    return &functions.back();
}
