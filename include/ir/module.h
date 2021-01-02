#pragma once

#include <string>
#include <map>
#include <type_traits>
#include <memory>
#include "utils/format.h"
#include "ir/value.h"

struct File;

namespace IR {
    class DeclSymbol {
    public:
        virtual ASTNS::AST* declAST() const = 0;
    };

    class Module {
    public:
        Module(File const &file);

        void addValue(std::string const &name, Value *t);
        void addDeclSymbol(std::string const &name, DeclSymbol *t);

        DeclSymbol* getDeclSymbol(std::string const &name);
        Value* getValue(std::string const &name);

    private:
        std::map<std::string, DeclSymbol*> decls;
        std::map<std::string, Value*> values;

        File const &file;
    };
}
