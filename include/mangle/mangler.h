#pragma once

#include <string>
#include "ir/value.h"

namespace Mangle {
    class NameMangler {
    public:
        std::string mangle_name(IR::Function const &f);
    };
}
