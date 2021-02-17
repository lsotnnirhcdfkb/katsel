#pragma once

#include <memory>

class Lexer;
struct File;
namespace ASTNS { class CUB; }

namespace Parse {
    std::unique_ptr<ASTNS::CUB> parse(Lexer &l, File &sourcefile);
}

