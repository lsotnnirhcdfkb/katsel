#include "../codegenlocal.h"
#include "utils/format.h"

CodeGen::FunctionCodeGen::FunctionCodeGen(CodeGen &cg, ASTNS::FunctionDecl *ast): curScope(0), cg(cg), ast(ast), exprCG(cg, *this), stmtCG(cg, *this) {}

bool CodeGen::FunctionCodeGen::codegen()
{
    return !errored;
}

void CodeGen::FunctionCodeGen::addLocal(std::string const &name, IR::Register *val)
{
    for (auto last = locals.rbegin(); last != locals.rend(); ++last)
        if (last->name == name && last->scopenum == curScope)
            reportAbortNoh(format("duplicate local added: \"%\"", name));

    Local l {curScope, val, name};
    locals.push_back(l);
}

CodeGen::FunctionCodeGen::Local* CodeGen::FunctionCodeGen::getLocal(std::string const &name)
{
    for (auto last = locals.rbegin(); last != locals.rend(); ++last)
        if (last->name == name)
            return &*last;

    return nullptr;
}

void CodeGen::FunctionCodeGen::incScope()
{
    ++curScope;
}
void CodeGen::FunctionCodeGen::decScope()
{
    --curScope;
    while (locals.size() && locals.back().scopenum > curScope) locals.pop_back();
}
