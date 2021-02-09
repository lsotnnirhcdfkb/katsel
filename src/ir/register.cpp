#include "ir/value.h"

IR::Register::Register(IR::Type const &ty, ASTNS::AST const &def_ast, bool mut, int id): id(id), mut(mut), ty(ty), _def_ast(def_ast) {}
IR::Type const &IR::Register::type() const {
    return *ty;
}

ASTNS::AST const &IR::Register::def_ast() const {
    return *_def_ast;
}
