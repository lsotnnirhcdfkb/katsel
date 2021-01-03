#pragma once

#include <string>
#include <map>
#include <type_traits>
#include <memory>
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

        virtual std::string name() const = 0;
    };

    class Module : public DeclSymbol {
    public:
        Module(std::string const &name, ASTNS::AST *declAST);

        void addValue(std::string const &name, Value *t) override;
        void addDeclSymbol(std::string const &name, DeclSymbol *t) override;

        DeclSymbol* getDeclSymbol(std::string const &name) const override;
        Value* getValue(std::string const &name) const override;

        ASTNS::AST* declAST() const override;

        std::string name() const override;

    private:
        std::map<std::string, DeclSymbol*> decls;
        std::map<std::string, Value*> values;

        ASTNS::AST *_declAST;

        std::string _name;
    };
}

inline std::ostream& operator<<(std::ostream &os, IR::DeclSymbol const *ds) {
    os << "'" << ds->name() << "'";
    return os;
}
