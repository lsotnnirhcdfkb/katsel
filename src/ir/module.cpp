#include "ir/module.h"
#include "utils/format.h"

IR::Module::Module(std::string const &name, ASTNS::AST *declAST): _declAST(declAST), _name(name) {}

ASTNS::AST* IR::Module::declAST() const {
    return _declAST;
}
std::string IR::Module::name() const {
    return _name;
}

IR::Value* IR::Module::getValue(std::string const &name) const {
    auto v = values.find(name);
    if (v == values.end())
        return nullptr;
    return v->second;
}
void IR::Module::addValue(std::string const &name, IR::Value *v) {
    if (values.find(name) != values.end())
        reportAbortNoh(format("add duplicate value under name %", name));

    values[name] = v;
}
IR::DeclSymbol* IR::Module::getDeclSymbol(std::string const &name) const {
    auto v = decls.find(name);
    if (v == decls.end())
        return nullptr;
    return v->second;
}
void IR::Module::addDeclSymbol(std::string const &name, IR::DeclSymbol *v) {
    if (decls.find(name) != decls.end())
        reportAbortNoh(format("add duplicate decl under name %", name));

    decls[name] = v;
}
