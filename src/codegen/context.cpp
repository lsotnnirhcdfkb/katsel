#include "codegen/codegen.h"

#include <iostream>
#include "message/errors.h"

CodeGenNS::Context::Context(std::string const &name): builder(context), mod(std::make_unique<llvm::Module>(name, context)) {}

Type* CodeGenNS::Context::getBuiltinType(BuiltinType::Builtins bty)
{
    for (std::unique_ptr<Type> &ty : types)
    {
        BuiltinType *b (dynamic_cast<BuiltinType*>(ty.get()));
        if (b && b->type == bty)
            return ty.get();
    }

    std::unique_ptr<Type> ty = std::make_unique<BuiltinType>(bty);

    Type *tyr = ty.get();
    types.push_back(std::move(ty));

    return tyr;
}

Type* CodeGenNS::Context::getFunctionType(Type *ret, std::vector<Type*> paramtys)
{
    for (std::unique_ptr<Type> &ty : types)
    {
        FunctionType *f (dynamic_cast<FunctionType*>(ty.get()));
        if (f && f->ret == ret && f->paramtys == paramtys)
            return ty.get();
    }

    std::unique_ptr<Type> ty = std::make_unique<FunctionType>(ret, paramtys);

    Type *tyr = ty.get();
    types.push_back(std::move(ty));

    return tyr;
}

Type* CodeGenNS::Context::getVoidType()
{
    for (std::unique_ptr<Type> &ty : types)
    {
        VoidType *v (dynamic_cast<VoidType*>(ty.get()));
        if (v)
            return ty.get();
    }

    std::unique_ptr<Type> ty = std::make_unique<VoidType>();

    Type *tyr = ty.get();
    types.push_back(std::move(ty));
    return tyr;
}

llvm::AllocaInst* CodeGenNS::Context::createEntryAlloca(llvm::Function *f, llvm::Type* type, std::string const &name)
{
    llvm::IRBuilder<> b (&f->getEntryBlock(), f->getEntryBlock().begin());
    return b.CreateAlloca(type, 0, name.c_str());
}

void CodeGenNS::Context::addLocal(std::string const &name, Type *type, llvm::AllocaInst *alloca, ASTNS::AST *ast)
{
    Value v (type, alloca, ast);
    Local l {curScope, v, name};
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

Value CodeGenNS::Context::findValue(std::string const &name)
{
    Local *l = findLocal(name);
    if (l)
        return l->v;

    return globalSymbolTable[name];
}

Value CodeGenNS::Context::findGlobal(std::string const &name)
{
    auto v = globalSymbolTable.find(name);
    if (v == globalSymbolTable.end())
        return Value();
    return v->second;
}
