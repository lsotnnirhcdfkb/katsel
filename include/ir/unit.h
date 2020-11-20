#pragma once

#include "ir/value/value.h"
#include "utils/file.h"

#include <string>

class Unit
{
public:
    Unit(File &file);

    File &file;
};
