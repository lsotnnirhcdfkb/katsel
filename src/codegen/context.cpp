#include "codegen/context.h"

Type* CodeGenContext::getBuiltinType(BuiltinType bty)
{
    for (std::unique_ptr<Type> &ty : types)
    {
        if (ty->typetype == TypeType::BUILTIN && ty->as.builtin == bty)
        {
            return ty.get();
        }
    }

    std::unique_ptr<Type> ty = std::make_unique<Type>();
    ty->typetype = TypeType::BUILTIN;
    ty->as.builtin = bty;

    Type *tyr = ty.get();
    types.push_back(std::move(ty));
    return tyr;
}

Type* CodeGenContext::getFunctionType(Type *ret, std::vector<Type*> paramtys)
{
    for (std::unique_ptr<Type> &ty : types)
    {
        if (ty->typetype == TypeType::FUNCTION && ty->as.function.ret == ret && ty->as.function.paramtys == paramtys)
        {
            return ty.get();
        }
    }

    std::unique_ptr<Type> ty = std::make_unique<Type>();
    ty->typetype = TypeType::FUNCTION;
    ty->as.function.ret = ret;
    ty->as.function.paramtys = paramtys;

    Type *tyr = ty.get();
    types.push_back(std::move(ty));
    return tyr;
}
