#pragma once

namespace ASTNS { class CUB; }
#include "ir/unit.h"
#include "utils/ptr.h"

namespace Codegen {
    Maybe<std::unique_ptr<IR::Unit>> codegen(File const &file, NNPtr<ASTNS::CUB> cub);
}
