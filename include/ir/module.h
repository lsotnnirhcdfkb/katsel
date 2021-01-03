#pragma once

#include <string>
#include <map>
#include <type_traits>
#include <memory>
#include "ir/value.h"

struct File;

#define DERIVE_DECLSYMBOL_DECL() \
    public:                                                                      \
        IR::Value *getValue(std::string const &name) const override;             \
        IR::DeclSymbol *getDeclSymbol(std::string const &name) const override;   \
        void addValue(std::string const &name, IR::Value *v) override;           \
        void addDeclSymbol(std::string const &name, IR::DeclSymbol *s) override; \
                                                                                 \
    private:                                                                     \
        std::map<std::string, IR::Value*> values;                                \
        std::map<std::string, IR::DeclSymbol*> decls;

#define DERIVE_DECLSYMBOL_DEF(cl) \
    IR::Value* cl::getValue(std::string const &name) const {                  \
        auto v = values.find(name);                                           \
        if (v == values.end())                                                \
            return nullptr;                                                   \
        return v->second;                                                     \
    }                                                                         \
    void cl::addValue(std::string const &name, IR::Value *v) {                \
        if (values.find(name) != values.end())                                \
            reportAbortNoh(format("add duplicate value under name %", name)); \
                                                                              \
        values[name] = v;                                                     \
    }                                                                         \
    IR::DeclSymbol* cl::getDeclSymbol(std::string const &name) const {        \
        auto v = decls.find(name);                                            \
        if (v == decls.end())                                                 \
            return nullptr;                                                   \
        return v->second;                                                     \
    }                                                                         \
    void cl::addDeclSymbol(std::string const &name, IR::DeclSymbol *v) {      \
        if (decls.find(name) != decls.end())                                  \
            reportAbortNoh(format("add duplicate decl under name %", name));  \
                                                                              \
        decls[name] = v;                                                      \
    }

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