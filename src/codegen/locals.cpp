#include "codegenlocal.h"

void Codegen::Helpers::Locals::add_local(std::string const &name, IR::Register &val) {
    for (auto last = locals.rbegin(); last != locals.rend(); ++last)
        if (last->name == name && last->scopenum == cur_scope)
            report_abort_noh(format("duplicate local added: \"{}\"", name));

    Local l {cur_scope, val, name};
    locals.push_back(l);
}

Maybe<Codegen::Helpers::Local> Codegen::Helpers::Locals::get_local(std::string const &name) {
    for (auto last = locals.rbegin(); last != locals.rend(); ++last)
        if (last->name == name)
            return *last;

    return Maybe<Local>();
}

void Codegen::Helpers::Locals::inc_scope() {
    ++cur_scope;
}
void Codegen::Helpers::Locals::dec_scope() {
    --cur_scope;
    while (locals.size() && locals.back().scopenum > cur_scope) locals.pop_back();
}
