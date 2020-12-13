#include "codegen/codegen.h"
#include "message/reportAbort.h"

#include <iostream>
#include "message/errors.h"

CodeGenNS::Context::Context(File const &file): unit(file), blackHoleBlock(std::make_unique<IR::Block>("blackHole", 0)), voidValue(getVoidType()) {}

IR::BuiltinType* CodeGenNS::Context::getBuiltinType(IR::BuiltinType::Builtins bty)
{
    for (std::unique_ptr<IR::Type> &ty : types)
    {
        IR::BuiltinType *b (dynamic_cast<IR::BuiltinType*>(ty.get()));
        if (b && b->type == bty)
            return b;
    }

    std::unique_ptr<IR::BuiltinType> ty = std::make_unique<IR::BuiltinType>(bty);

    IR::BuiltinType *tyr = ty.get();
    types.push_back(std::move(ty));

    return tyr;
}

IR::FunctionType* CodeGenNS::Context::getFunctionType(IR::Type *ret, std::vector<IR::Type*> paramtys)
{
    for (std::unique_ptr<IR::Type> &ty : types)
    {
        IR::FunctionType *f (dynamic_cast<IR::FunctionType*>(ty.get()));
        if (f && f->ret == ret && f->paramtys == paramtys)
            return f;
    }

    std::unique_ptr<IR::FunctionType> ty = std::make_unique<IR::FunctionType>(ret, paramtys);

    IR::FunctionType *tyr = ty.get();
    types.push_back(std::move(ty));

    return tyr;
}

IR::VoidType* CodeGenNS::Context::getVoidType()
{
    for (std::unique_ptr<IR::Type> &ty : types)
    {
        IR::VoidType *v (dynamic_cast<IR::VoidType*>(ty.get()));
        if (v)
            return v;
    }

    std::unique_ptr<IR::VoidType> ty = std::make_unique<IR::VoidType>();

    IR::VoidType *tyr = ty.get();
    types.push_back(std::move(ty));
    return tyr;
}

void CodeGenNS::Context::addLocal(std::string const &name, IR::Register *reg)
{
    Local l {curScope, reg, name};
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

CodeGenNS::Context::Local* CodeGenNS::Context::findLocal(std::string const &name)
{
    for (auto last = locals.rbegin(); last != locals.rend(); ++last)
        if (last->name == name)
            return &*last;

    return nullptr;
}

IR::Value* CodeGenNS::Context::findValue(std::string const &name)
{
    Local *l = findLocal(name);
    if (l)
        return l->v;

    return globalSymbolTable[name];
}

IR::Value* CodeGenNS::Context::findGlobal(std::string const &name)
{
    auto v = globalSymbolTable.find(name);
    if (v == globalSymbolTable.end())
        return nullptr;
    return v->second;
}

IR::ConstInt* CodeGenNS::Context::getConstInt(IR::BuiltinType *ty, int val)
{
    std::unique_ptr<IR::ConstInt> ci = std::make_unique<IR::ConstInt>(ty, val);
    IR::ConstInt *ciraw = ci.get();
    constants.push_back(std::move(ci));

    return ciraw;
}
IR::Void* CodeGenNS::Context::getVoidValue()
{
    return &voidValue;
}
