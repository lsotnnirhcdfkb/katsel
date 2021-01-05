#pragma once

#include "ir/module.h"

#include <vector>
#include "llvm/Support/raw_ostream.h"

struct File;

namespace IR {
    class Function;
    class FunctionType;

    class Unit {
    public:
        Unit(File const &file);

        void print(llvm::raw_ostream &ostream);

        std::unique_ptr<ASTNS::ImplicitDecl> implicitDeclAST;

        File const &file;
        Module mod;

        std::vector<std::unique_ptr<Function>> functions;
    };
}
