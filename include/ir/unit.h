#pragma once

#include "utils/file.h"
#include "ir/value.h"

#include <string>
#include <vector>
#include <ostream>

class Unit
{
public:
    Unit(File const &file);

    void print(std::ostream &ostream) const;
    void cfgDot(std::ostream &ostream) const;

    Function* addFunction(FunctionType *type, std::string name, ASTNS::Function *ast);

private:
    std::vector<std::unique_ptr<Function>> functions;
    File const &file;
};
