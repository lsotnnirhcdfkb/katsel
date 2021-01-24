#pragma once

#include <string>
#include <map>
#include "utils/maybe.h"
#include "ir/value.h"

struct File;

#define DERIVE_DECLSYMBOL_ITEMS_DECL() \
    public: \
        Maybe<NNPtr<IR::Value>> get_value(std::string const &name) const override; \
        Maybe<NNPtr<IR::DeclSymbol>> get_decl_symbol(std::string const &name) const override; \
        void add_value(std::string const &name, NNPtr<IR::Value> v) override; \
        void add_decl_symbol(std::string const &name, NNPtr<IR::DeclSymbol> s) override; \
        std::map<std::string, NNPtr<IR::Value>> get_values() const override; \
        std::map<std::string, NNPtr<DeclSymbol>> get_decl_symbols() const override; \
    private: \
        std::map<std::string, NNPtr<IR::Value>> values; \
        std::map<std::string, NNPtr<IR::DeclSymbol>> decls;

#define DERIVE_DECLSYMBOL_ITEMS_IMPL(cl) \
    Maybe<NNPtr<IR::Value>> cl::get_value(std::string const &name) const { \
        auto v = values.find(name); \
        if (v == values.end()) \
            return Maybe<NNPtr<IR::Value>>(); \
        return NNPtr<IR::Value>(v->second); \
    } \
    void cl::add_value(std::string const &name, NNPtr<IR::Value> v) { \
        if (values.find(name) != values.end()) \
            report_abort_noh(format("add duplicate value under name {}", name)); \
        values.emplace(name, v); \
    } \
    Maybe<NNPtr<IR::DeclSymbol>> cl::get_decl_symbol(std::string const &name) const { \
        auto v = decls.find(name); \
        if (v == decls.end()) \
            return Maybe<NNPtr<IR::DeclSymbol>>(); \
        return NNPtr<IR::DeclSymbol>(v->second); \
    } \
    void cl::add_decl_symbol(std::string const &name, NNPtr<IR::DeclSymbol> v) { \
        if (decls.find(name) != decls.end()) \
            report_abort_noh(format("add duplicate decl under name {}", name)); \
        decls.emplace(name, v); \
    } \
    std::map<std::string, NNPtr<IR::Value>> cl::get_values() const { \
        return values; \
    } \
    std::map<std::string, NNPtr<IR::DeclSymbol>> cl::get_decl_symbols() const { \
        return decls; \
    }

namespace IR {
#define DECLSYM_CLASS_LIST(macro) \
        macro(Type) \
        macro(Module)
    class DeclSymbolVisitor;
    class DeclSymbol {
    public:
        virtual NNPtr<ASTNS::AST> decl_ast() const = 0;
        virtual std::string name() const = 0;

        virtual void add_value(std::string const &name, NNPtr<Value> t) = 0;
        virtual void add_decl_symbol(std::string const &name, NNPtr<DeclSymbol> t) = 0;

        virtual Maybe<NNPtr<DeclSymbol>> get_decl_symbol(std::string const &name) const = 0;
        virtual Maybe<NNPtr<Value>> get_value(std::string const &name) const = 0;

        virtual std::map<std::string, NNPtr<Value>> get_values() const = 0;
        virtual std::map<std::string, NNPtr<DeclSymbol>> get_decl_symbols() const = 0;

        virtual void declsym_accept(DeclSymbolVisitor &v) = 0;
    };

    class Module : public DeclSymbol {
    public:
        Module(std::string const &name, NNPtr<ASTNS::AST> decl_ast);

        NNPtr<ASTNS::AST> decl_ast() const override;
        std::string name() const override;

        virtual void declsym_accept(DeclSymbolVisitor &v) override;

        DERIVE_DECLSYMBOL_ITEMS_DECL()

    private:
        NNPtr<ASTNS::AST> _decl_ast;

        std::string _name;
    };

    class DeclSymbolVisitor {
    public:
        virtual ~DeclSymbolVisitor() {}
#define VISITMETHOD(cl) \
        virtual void declsym_visit##cl(cl &ds) = 0;
        DECLSYM_CLASS_LIST(VISITMETHOD)
#undef VISITMETHOD
    };
}

std::ostream& operator<<(std::ostream &os, NNPtr<IR::DeclSymbol const> ds);
