#include "ir/module.h"
#include "utils/format.h"

IR::Module::Module(std::string const &name, ASTNS::AST *declAST): _declAST(declAST), _name(name) {}

ASTNS::AST* IR::Module::declAST() const {
    return _declAST;
}
std::string IR::Module::name() const {
    return _name;
}

DERIVE_DECLSYMBOL_DEF(IR::Module)
