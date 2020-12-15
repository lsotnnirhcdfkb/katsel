#include "codegenlocal.h"

CodeGen::Context::Context(): voidValue(getVoidType()) {}

// getting types {{{1
IR::BuiltinType* CodeGen::Context::getBuiltinType(IR::BuiltinType::Builtins bty)
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

IR::FunctionType* CodeGen::Context::getFunctionType(IR::Type *ret, std::vector<IR::Type*> paramtys)
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

IR::VoidType* CodeGen::Context::getVoidType()
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

// other {{{1
IR::Value* CodeGen::Context::getGlobal(std::string const &name)
{
    auto v = globalSymbolTable.find(name);
    if (v == globalSymbolTable.end())
        return nullptr;
    return v->second;
}

IR::ConstInt* CodeGen::Context::getConstInt(IR::BuiltinType *ty, int val)
{
    std::unique_ptr<IR::ConstInt> ci = std::make_unique<IR::ConstInt>(ty, val);
    IR::ConstInt *ciraw = ci.get();
    constants.push_back(std::move(ci));

    return ciraw;
}
IR::Void* CodeGen::Context::getVoidValue()
{
    return &voidValue;
}
