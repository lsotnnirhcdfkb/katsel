#include "mangle/mangler.h"
#include <sstream>
#include <string>
#include <memory>
#include "ir/value.h"
#include "ir/type.h"
#include "message/reportAbort.h"

struct Component
{
    virtual ~Component() = default;
    virtual void stringify(std::stringstream &ss) = 0;
};

// TypeComponent {{{1
struct TypeComponent : public Component
{
    IR::Type *ty;
    TypeComponent(IR::Type *ty): ty(ty) {}

    void stringify(std::stringstream &ss) override
    {
        ss << "T";
        // float    - f
        // double   - d
        // char     - c
        // bool     - b
        // function - ...
        // void     - v
        // uint8    - u
        // uint16   - w
        // uint32   - x
        // uint64   - y
        // sint8    - s
        // sint16   - r
        // sint32   - q
        // sint64   - p

#define CHECKTY(t) if (IR::t *as##t = dynamic_cast<IR::t*>(ty))
        CHECKTY(FloatType)
        {
            ss << (asFloatType->size == 32 ? 'f' : 'd');
        }
        CHECKTY(IntType)
        {
            switch (asIntType->size)
            {
                case 8:  ss << (asIntType->isSigned ? 's' : 'u'); break;
                case 16: ss << (asIntType->isSigned ? 'r' : 'w'); break;
                case 32: ss << (asIntType->isSigned ? 'q' : 'x'); break;
                case 64: ss << (asIntType->isSigned ? 'p' : 'y'); break;
            }
        }
        CHECKTY(CharType)
            ss << 'c';
        CHECKTY(BoolType)
            ss << 'b';
        CHECKTY(FunctionType)
        {
            ss << 'F';
            TypeComponent(asFunctionType->ret).stringify(ss);
            for (IR::Type *pty : asFunctionType->paramtys)
                TypeComponent(pty).stringify(ss);
            ss << 'f';
        }
        CHECKTY(VoidType)
            ss << 'v';
        CHECKTY(GenericIntType)
            ss << 'x';
        CHECKTY(GenericFloatType)
            ss << 'f';
#undef CHECKTY
        ss << "t";
    }
};

// FunctionComponent {{{1
struct FunctionComponent : public Component
{
    std::string name;
    std::vector<std::unique_ptr<TypeComponent>> types;

    FunctionComponent(IR::Function const &f): name(f.name)
    {
        for (IR::Type *ty : f.ty->paramtys)
            types.push_back(std::make_unique<TypeComponent>(ty));
    }

    void stringify(std::stringstream &ss) override
    {
        ss << "F" << name.size() << name;

        for (std::unique_ptr<TypeComponent> &ty : types)
            ty->stringify(ss);

        ss << "f";
    }
};
// }}}1

struct MangledPath
{
    std::unique_ptr<Component> component;
    std::string stringify()
    {
        std::stringstream ss;
        ss << "_ksl_";
        component->stringify(ss);
        return ss.str();
    }
};

std::string Mangle::NameMangler::mangleName(IR::Function &f)
{
    MangledPath p;
    p.component = std::make_unique<FunctionComponent>(f);
    return p.stringify();
}
