#pragma once

namespace ASTNS { class CUB; }
#include "ir/unit.h"
#include "utils/ptr.h"

namespace CodeGen {
    Maybe<std::unique_ptr<IR::Unit>> codegen(NNPtr<ASTNS::CUB> cub);
}
