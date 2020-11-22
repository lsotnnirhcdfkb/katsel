#include "codegen/codegen.h"

#include <iostream>
#include "message/errors.h"

CodeGenNS::Context::Context(File const &file): unit(file) {}

BuiltinType* CodeGenNS::Context::getBuiltinType(BuiltinType::Builtins bty)
{
    for (std::unique_ptr<Type> &ty : types)
    {
        BuiltinType *b (dynamic_cast<BuiltinType*>(ty.get()));
        if (b && b->type == bty)
            return b;
    }

    std::unique_ptr<BuiltinType> ty = std::make_unique<BuiltinType>(bty);

    BuiltinType *tyr = ty.get();
    types.push_back(std::move(ty));

    return tyr;
}

FunctionType* CodeGenNS::Context::getFunctionType(Type *ret, std::vector<Type*> paramtys)
{
    for (std::unique_ptr<Type> &ty : types)
    {
        FunctionType *f (dynamic_cast<FunctionType*>(ty.get()));
        if (f && f->ret == ret && f->paramtys == paramtys)
            return f;
    }

    std::unique_ptr<FunctionType> ty = std::make_unique<FunctionType>(ret, paramtys);

    FunctionType *tyr = ty.get();
    types.push_back(std::move(ty));

    return tyr;
}

VoidType* CodeGenNS::Context::getVoidType()
{
    for (std::unique_ptr<Type> &ty : types)
    {
        VoidType *v (dynamic_cast<VoidType*>(ty.get()));
        if (v)
            return v;
    }

    std::unique_ptr<VoidType> ty = std::make_unique<VoidType>();

    VoidType *tyr = ty.get();
    types.push_back(std::move(ty));
    return tyr;
}

void CodeGenNS::Context::addLocal(std::string const &name, Value *val)
{
    Local l {curScope, val, name};
    locals.push_back(l);
}

void CodeGenNS::Context::incScope()
{
    ++curScope;

    if (curScope == 0) // default curScope value is 1
        reportAbortNoh("Scope index overflowed to 0 (too many nested scopes)");
}

void CodeGenNS::Context::decScope()
{
    --curScope;
    if (curScope == 0)
        reportAbortNoh("Scope index reached 0 (compiler messed up)");

    while (locals.size() && locals.back().scopenum > curScope) locals.pop_back();
}

Local* CodeGenNS::Context::findLocal(std::string const &name)
{
    for (auto last = locals.rbegin(); last != locals.rend(); ++last)
        if (last->name == name)
            return &*last;

    return nullptr;
}

Value* CodeGenNS::Context::findValue(std::string const &name)
{
    Local *l = findLocal(name);
    if (l)
        return l->v;

    return globalSymbolTable[name];
}

Value* CodeGenNS::Context::findGlobal(std::string const &name)
{
    auto v = globalSymbolTable.find(name);
    if (v == globalSymbolTable.end())
        return nullptr;
    return v->second;
}

ConstInt* CodeGenNS::Context::getConstInt(BuiltinType *ty, int val, ASTNS::AST *ast)
{
    std::unique_ptr<ConstInt> ci = std::make_unique<ConstInt>(ty, ast, val);
    ConstInt *ciraw = ci.get();
    constants.push_back(std::move(ci));

    return ciraw;
}
