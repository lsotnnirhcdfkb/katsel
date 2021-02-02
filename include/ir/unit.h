#pragma once

#include <vector>
#include <memory>

#include "ir/module.h"
#include "codegen/context.h"

namespace llvm { class raw_ostream; }
struct File;

namespace IR {
    class Function;

    class Unit {
    public:
        Unit(File const &file);
        ~Unit();

        void print(llvm::raw_ostream &ostream) const;

        Codegen::Context context;

        NNPtr<File const> file;
        Module mod;

        std::vector<std::unique_ptr<Function>> functions;
    };
}
