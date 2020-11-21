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

private:
    std::vector<Function> functions;
    File const &file;
};
