#pragma once

#include <string>
#include <map>
#include "utils/maybe.h"
#include "ir/value.h"

struct File;

#define DERIVE_DECLSYMBOL_ITEMS_DECL() \
    public: \
        Maybe<IR::Value&> get_value(std::string const &name) const override; \
        Maybe<IR::DeclSymbol&> get_decl_symbol(std::string const &name) const override; \
        void add_value(std::string const &name, IR::Value &v) override; \
        void add_decl_symbol(std::string const &name, IR::DeclSymbol &s) override; \
        std::map<std::string, NNPtr<IR::Value>> get_values() const override; \
        std::map<std::string, NNPtr<DeclSymbol>> get_decl_symbols() const override; \
    private: \
        std::map<std::string, NNPtr<IR::Value>> values; \
        std::map<std::string, NNPtr<IR::DeclSymbol>> decls;

#define DERIVE_DECLSYMBOL_ITEMS_IMPL(cl) \
    Maybe<IR::Value&> cl::get_value(std::string const &name) const { \
        auto v = values.find(name); \
        if (v == values.end()) \
            return Maybe<IR::Value&>(); \
        return *v->second; \
    } \
    void cl::add_value(std::string const &name, IR::Value &v) { \
        if (values.find(name) != values.end()) \
            report_abort_noh(format("add duplicate value under name {}", name)); \
        values.emplace(name, v); \
    } \
    Maybe<IR::DeclSymbol&> cl::get_decl_symbol(std::string const &name) const { \
        auto v = decls.find(name); \
        if (v == decls.end()) \
            return Maybe<IR::DeclSymbol&>(); \
        return *v->second; \
    } \
    void cl::add_decl_symbol(std::string const &name, IR::DeclSymbol &v) { \
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

#define DERIVE_DECLSYMBOL_AST_DECL() \
    private: \
        Maybe<NNPtr<ASTNS::AST const>> _decl_ast; \
        void _init_decl_ast(Maybe<ASTNS::AST const &> &a);

#define DERIVE_DECLSYMBOL_AST_IMPL(cl) \
    Maybe<ASTNS::AST const &> cl::decl_ast() const { \
        return _decl_ast.has() ? *_decl_ast.get() : Maybe<ASTNS::AST const &>(); \
    } \
    void cl::_init_decl_ast(Maybe<ASTNS::AST const &> &a) { \
        _decl_ast = a.has() ? Maybe<NNPtr<ASTNS::AST const>>(a.get()) : Maybe<NNPtr<ASTNS::AST const>>(); \
    }


namespace IR {
#define DECLSYM_CLASS_LIST(macro) \
        macro(Type) \
        macro(Module)
    class DeclSymbolVisitor;
    class DeclSymbol {
    public:
        virtual Maybe<ASTNS::AST const &> decl_ast() const = 0;
        virtual std::string name() const = 0;

        virtual void add_value(std::string const &name, Value &t) = 0;
        virtual void add_decl_symbol(std::string const &name, DeclSymbol &t) = 0;

        virtual Maybe<DeclSymbol&> get_decl_symbol(std::string const &name) const = 0;
        virtual Maybe<Value&> get_value(std::string const &name) const = 0;

        virtual std::map<std::string, NNPtr<Value>> get_values() const = 0;
        virtual std::map<std::string, NNPtr<DeclSymbol>> get_decl_symbols() const = 0;

        virtual void declsym_accept(DeclSymbolVisitor &v) const = 0;
    };

    class Module : public DeclSymbol {
    public:
        Module(std::string const &name, Maybe<ASTNS::AST const &> decl_ast);

        Maybe<ASTNS::AST const &> decl_ast() const override;
        std::string name() const override;

        virtual void declsym_accept(DeclSymbolVisitor &v) const override;

        DERIVE_DECLSYMBOL_ITEMS_DECL()

    private:
        DERIVE_DECLSYMBOL_AST_DECL()

        std::string _name;
    };

    class DeclSymbolVisitor {
    public:
        virtual ~DeclSymbolVisitor() {}
#define VISITMETHOD(cl) \
        virtual void declsym_visit(cl const &ds) = 0;
        DECLSYM_CLASS_LIST(VISITMETHOD)
#undef VISITMETHOD
    };
}

std::ostream& operator<<(std::ostream &os, IR::DeclSymbol const &ds);
