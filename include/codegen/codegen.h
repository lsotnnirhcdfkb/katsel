#pragma once

namespace ASTNS { class CUB; }
#include "ir/unit.h"
#include "utils/ptr.h"

namespace Codegen {
    Maybe<IR::Unit> codegen(File const &file, NNPtr<ASTNS::CUB> cub);
}
