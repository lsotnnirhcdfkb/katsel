#pragma once

#include <vector>
#include <memory>

#include "ir/module.h"

namespace llvm { class raw_ostream; }
struct File;

namespace IR {
    class Function;
    class FunctionType;

    class Unit {
    public:
        Unit(File const &file, ASTNS::ImplicitDecl &implicit);

        void print(llvm::raw_ostream &ostream);

        File const &file;
        Module mod;

        std::vector<std::unique_ptr<Function>> functions;
    };
}
