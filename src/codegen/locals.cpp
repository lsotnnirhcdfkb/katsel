#include "codegenlocal.h"

void CodeGen::Helpers::Locals::add_local(std::string const &name, IR::Instrs::Register &val) {
    for (auto last = locals.rbegin(); last != locals.rend(); ++last)
        if (last->name == name && last->scopenum == cur_scope)
            report_abort_noh(format("duplicate local added: \"{}\"", name));

    Local l {cur_scope, val, name};
    locals.push_back(l);
}

Maybe<CodeGen::Helpers::Local> CodeGen::Helpers::Locals::get_local(std::string const &name) {
    for (auto last = locals.rbegin(); last != locals.rend(); ++last)
        if (last->name == name)
            return *last;

    return Maybe<Local>();
}

void CodeGen::Helpers::Locals::inc_scope() {
    ++cur_scope;
}
void CodeGen::Helpers::Locals::dec_scope() {
    --cur_scope;
    while (locals.size() && locals.back().scopenum > cur_scope) locals.pop_back();
}
