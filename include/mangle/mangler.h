#pragma once

#include <string>
#include "ir/value.h"

namespace Mangle {
    class NameMangler {
    public:
        std::string mangleName(IR::Function &f);
    };
}
