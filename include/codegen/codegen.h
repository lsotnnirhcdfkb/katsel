#pragma once

namespace ASTNS { class CUB; }
#include "ir/unit.h"
#include "utils/ptr.h"

namespace CodeGen {
    Maybe<IR::Unit> codegen(NNPtr<ASTNS::CUB> cub);
}
