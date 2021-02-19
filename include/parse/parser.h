#pragma once

#include <memory>
#include "utils/maybe.h"

class Lexer;
struct File;
namespace ASTNS { class CU; }

namespace Parse {
    Maybe<std::unique_ptr<ASTNS::CU>> parse(Lexer &l, File &sourcefile);
}

