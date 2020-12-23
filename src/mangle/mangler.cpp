#include "mangle/mangler.h"
#include <sstream>
#include <string>
#include <memory>
#include "ir/value.h"
#include "ir/type.h"
#include "message/reportAbort.h"

static inline void mangleIdentifier(std::stringstream &ss, std::string i)
{
    ss << i.size() << i;
}

static void mangleIntType(std::stringstream &ss, IR::IntType *ty)
{
    switch (ty->size)
    {
        case 8:  ss << (ty->isSigned ? 's' : 'u'); break;
        case 16: ss << (ty->isSigned ? 'r' : 'w'); break;
        case 32: ss << (ty->isSigned ? 'q' : 'x'); break;
        case 64: ss << (ty->isSigned ? 'p' : 'y'); break;
    }
}
static void mangleFloatType(std::stringstream &ss, IR::FloatType *ty)
{
    ss << (ty->size == 32 ? 'f' : 'd');
}
static void mangleCharType(std::stringstream &ss, IR::CharType *ty)
{
    ss << 'c';
}
static void mangleBoolType(std::stringstream &ss, IR::BoolType *ty)
{
    ss << 'b';
}
static void mangleType(std::stringstream &ss, IR::Type *ty);
static void mangleFunctionType(std::stringstream &ss, IR::FunctionType *ty)
{
    ss << 'F';
    mangleType(ss, ty->ret);
    for (IR::Type *pty : ty->paramtys)
        mangleType(ss, pty);
    ss << 'f';
}
static void mangleVoidType(std::stringstream &ss, IR::VoidType *ty)
{
    ss << 'v';
}
static void mangleGenericIntType(std::stringstream &ss, IR::GenericIntType *ty)
{
    ss << 'x';
}
static void mangleGenericFloatType(std::stringstream &ss, IR::GenericFloatType *ty)
{
    ss << 'f';
}

static void mangleType(std::stringstream &ss, IR::Type *ty)
{
    ss << "T";
#define CHECKTY(t) if (IR::t *as##t = dynamic_cast<IR::t*>(ty)) mangle##t(ss, as##t);
    CHECKTY(FloatType)
    CHECKTY(IntType)
    CHECKTY(CharType)
    CHECKTY(BoolType)
    CHECKTY(FunctionType)
    CHECKTY(VoidType)
    CHECKTY(GenericIntType)
    CHECKTY(GenericFloatType)
#undef CHECKTY
    ss << "t";
}

static void mangleFunction(std::stringstream &ss, IR::Function &f)
{
    ss << "F";

    mangleIdentifier(ss, f.name);
    for (IR::Type *ty : f.ty->paramtys)
        mangleType(ss, ty);

    ss << "f";
}

static std::string manglePath(IR::Function &f)
{
    std::stringstream ss;
    ss << "_ksl_";
    mangleFunction(ss, f);
    return ss.str();
}

std::string Mangle::NameMangler::mangleName(IR::Function &f)
{
    return manglePath(f);
}
