#include "lower/lowerer.h"
#include "lowererlocal.h"
#include "ir/module.h"
#include "ir/type.h"

using namespace Lower;

LowerDeclSym::LowerDeclSym(Lowerer &lowerer): lowerer(lowerer) {}

void LowerDeclSym::declsym_visit(IR::Module const &mod) {
    walk(mod);
}
void LowerDeclSym::declsym_visit(IR::Type const &ty) {
    walk(ty);
}

void LowerDeclSym::walk(IR::DeclSymbol const &ds) {
    for (auto child_ds_entry : ds.get_decl_symbols()) {
        NNPtr<IR::DeclSymbol> child_ds = child_ds_entry.second;
        child_ds->declsym_accept(*this);
    }
    LowerValueDef value_def(lowerer);
    for (auto child_val_entry : ds.get_values()) {
        NNPtr<IR::Value> child_val = child_val_entry.second;
        child_val->value_accept(value_def);
    }
}
