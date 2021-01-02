#pragma once

#include "ir/module.h"

#include <string>
#include <vector>
#include "llvm/Support/raw_ostream.h"

struct File;

namespace IR {
    class Function;
    class FunctionType;

    class Unit {
    public:
        Unit(File const &file);

        void print(llvm::raw_ostream &ostream) const;
        void cfgDot(llvm::raw_ostream &ostream) const;

        File const &file;
        Module mod;

        std::vector<std::unique_ptr<Function>> functions;
    };
}
