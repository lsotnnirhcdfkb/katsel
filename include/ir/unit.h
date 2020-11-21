#pragma once

#include "utils/file.h"
#include "ir/function.h"

#include <string>
#include <vector>

class Unit
{
public:
    Unit(File const &file);

private:
    std::vector<Function> functions;
    File const &file;
};
