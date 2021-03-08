#include "ir/module.h"
#include "utils/format.h"
#include "ir/type.h"
#include <ostream>

IR::Module::Module(std::string const &name, Maybe<ASTNS::AST const &> decl_ast): _name(name) {
    _init_decl_ast(decl_ast);
}

std::string IR::Module::name() const {
    return _name;
}

DERIVE_DECLSYMBOL_ITEMS_IMPL(IR::Module)
DERIVE_DECLSYMBOL_AST_IMPL(IR::Module)

// do the dsaccept methods here, even though that doesn't really make sense with the file structure but whatever
#define DSACCEPTMETHOD(cl) \
    void IR::cl::declsym_accept(IR::DeclSymbolVisitor &v) const { v.declsym_visit(*this); }
DECLSYM_CLASS_LIST(DSACCEPTMETHOD)
#undef DSACCEPTMETHOD

std::ostream& operator<<(std::ostream &os, IR::DeclSymbol const &ds) {
    os << "'" << ds.name() << "'";
    return os;
}
