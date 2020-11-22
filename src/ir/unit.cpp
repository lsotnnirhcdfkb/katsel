#include "ir/unit.h"

Unit::Unit(File const &file): file(file) {}

void Unit::print(std::ostream &ostream) const
{
    ostream << "> Unit \"" << file.filename << "\"" << std::endl;
    for (std::unique_ptr<Function> const &f : functions)
        f->definition(ostream);
}

void Unit::cfgDot(std::ostream &ostream) const
{
    ostream << "strict digraph \"CFG for unit " << file.filename << "\" {\n";
    for (std::unique_ptr<Function> const &f : functions)
        f->cfgDot(ostream);
    ostream << "}\n";
}

Function* Unit::addFunction(FunctionType *type, std::string name, ASTNS::Function *ast)
{
    std::unique_ptr<Function> f (std::make_unique<Function>(type, name, ast));
    Function *fraw = f.get();
    functions.push_back(std::move(f));
    return fraw;
}
