#pragma once

#include "utils/file.h"
#include "ir/value.h"

#include <string>
#include <vector>
#include "llvm/Support/raw_ostream.h"

namespace IR {
    class Unit {
    public:
        Unit(File const &file);

        void print(llvm::raw_ostream &ostream) const;
        void cfgDot(llvm::raw_ostream &ostream) const;

        IR::Function* addFunction(FunctionType *type, std::string name, ASTNS::FunctionDecl *ast);

        File const &file;
        std::vector<std::unique_ptr<IR::Function>> functions;
    };
}
