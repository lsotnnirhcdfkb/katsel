#include "ir/value.h"

IR::Register::Register(IR::Type const &ty, Span const &def_span, bool mut, int id): id(id), mut(mut), ty(ty), _def_span(def_span) {}
IR::Type const &IR::Register::type() const {
    return *ty;
}

Span const &IR::Register::def_span() const {
    return _def_span;
}
