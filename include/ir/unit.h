#pragma once

#include <vector>
#include <memory>

#include "ir/module.h"

namespace llvm { class raw_ostream; }
struct File;
namespace ASTNS { class ImplicitDecl; }

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
