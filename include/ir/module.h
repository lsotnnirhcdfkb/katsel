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

        virtual void addValue(std::string const &name, Value *t) = 0;
        virtual void addDeclSymbol(std::string const &name, DeclSymbol *t) = 0;

        virtual DeclSymbol* getDeclSymbol(std::string const &name) const = 0;
        virtual Value* getValue(std::string const &name) const = 0;
    };

    class Module : public DeclSymbol {
    public:
        Module(File const &file, ASTNS::AST *declAST);

        void addValue(std::string const &name, Value *t) override;
        void addDeclSymbol(std::string const &name, DeclSymbol *t) override;

        DeclSymbol* getDeclSymbol(std::string const &name) const override;
        Value* getValue(std::string const &name) const override;

        ASTNS::AST* declAST() const override;

    private:
        std::map<std::string, DeclSymbol*> decls;
        std::map<std::string, Value*> values;

        ASTNS::AST *_declAST;

        File const &file;
    };
}
