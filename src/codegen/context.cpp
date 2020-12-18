#include "codegenlocal.h"
#include "utils/format.h"

CodeGen::Context::Context(): voidValue(getVoidType()) {}

// getting types {{{1 TODO: make a template function to loop through things and either make operator== = default for all types or use a lambda to compare them
#define GET_TYPE_DEF(type) IR::type* CodeGen::Context::get##type
#define LOOP_TYPES() for (std::unique_ptr<IR::Type> &ty : types)
#define CHECK_TYPE_TYPE(type) IR::type *casted (dynamic_cast<IR::type*>(ty.get()));
#define CHECK_FIELD(field) casted->field == field
#define CONSTRUCT_TYPE(type) std::unique_ptr<IR::type> ty = std::make_unique<IR::type>
#define PUSH_RETURN(type) \
    IR::type *tyr (ty.get());       \
    types.push_back(std::move(ty)); \
    return tyr;

GET_TYPE_DEF(FloatType)(int size)
{
    LOOP_TYPES()
    {
        CHECK_TYPE_TYPE(FloatType)
        if (casted && CHECK_FIELD(size)) return casted;
    }
    CONSTRUCT_TYPE(FloatType)(size);
    PUSH_RETURN(FloatType)
}
GET_TYPE_DEF(IntType)(int size, bool isSigned)
{
    LOOP_TYPES()
    {
        CHECK_TYPE_TYPE(IntType)
        if (casted && CHECK_FIELD(size) && CHECK_FIELD(isSigned)) return casted;
    }
    CONSTRUCT_TYPE(IntType)(size, isSigned);
    PUSH_RETURN(IntType)
}
GET_TYPE_DEF(CharType)()
{
    LOOP_TYPES()
    {
        CHECK_TYPE_TYPE(CharType)
        if (casted) return casted;
    }
    CONSTRUCT_TYPE(CharType)();
    PUSH_RETURN(CharType)
}
GET_TYPE_DEF(BoolType)()
{
    LOOP_TYPES()
    {
        CHECK_TYPE_TYPE(BoolType)
        if (casted) return casted;
    }
    CONSTRUCT_TYPE(BoolType)();
    PUSH_RETURN(BoolType)
}
GET_TYPE_DEF(GenericIntType)()
{
    LOOP_TYPES()
    {
        CHECK_TYPE_TYPE(GenericIntType)
        if (casted) return casted;
    }
    CONSTRUCT_TYPE(GenericIntType)();
    PUSH_RETURN(GenericIntType)
}
GET_TYPE_DEF(GenericFloatType)()
{
    LOOP_TYPES()
    {
        CHECK_TYPE_TYPE(GenericFloatType)
        if (casted) return casted;
    }
    CONSTRUCT_TYPE(GenericFloatType)();
    PUSH_RETURN(GenericFloatType)
}
GET_TYPE_DEF(FunctionType)(IR::Type *ret, std::vector<IR::Type*> paramtys)
{
    LOOP_TYPES()
    {
        CHECK_TYPE_TYPE(FunctionType)
        if (casted && CHECK_FIELD(ret) && CHECK_FIELD(paramtys)) return casted;
    }
    CONSTRUCT_TYPE(FunctionType)(ret, paramtys);
    PUSH_RETURN(FunctionType)
}

GET_TYPE_DEF(VoidType)()
{
    LOOP_TYPES()
    {
        CHECK_TYPE_TYPE(VoidType)
        if (casted) return casted;
    }
    CONSTRUCT_TYPE(VoidType)();
    PUSH_RETURN(VoidType)
}
#undef GET_TYPE_DEF
#undef LOOP_TYPES
#undef CHECK_TYPE_TYPE
#undef CHECK_FIELD
#undef CONSTRUCT_TYPE
#undef PUSH_RETURN
// getting values {{{1
template <typename Ret, typename ... Args>
static Ret* getConstVal(std::vector<std::unique_ptr<IR::Value>> &constants, Args ...args)
{
    std::unique_ptr<Ret> cv = std::make_unique<Ret>(args...);
    Ret *cvraw = cv.get();
    constants.push_back(std::move(cv));
    return cvraw;
}
IR::ConstFloat* CodeGen::Context::getConstFloat(IR::FloatType *ty, double value)
{
    return getConstVal<IR::ConstFloat>(constants, ty, value);
}
IR::ConstInt* CodeGen::Context::getConstInt(IR::IntType *ty, uint64_t value)
{
    return getConstVal<IR::ConstInt>(constants, ty, value);
}
IR::ConstFloat* CodeGen::Context::getConstFloat(IR::GenericFloatType *ty, double value)
{
    return getConstVal<IR::ConstFloat>(constants, ty, value);
}
IR::ConstInt* CodeGen::Context::getConstInt(IR::GenericIntType *ty, uint64_t value)
{
    return getConstVal<IR::ConstInt>(constants, ty, value);
}
IR::ConstChar* CodeGen::Context::getConstChar(uint8_t value)
{
    return getConstVal<IR::ConstChar>(constants, getCharType(), value);
}
IR::ConstBool* CodeGen::Context::getConstBool(bool value)
{
    return getConstVal<IR::ConstBool>(constants, getBoolType(), value);
}
IR::Void* CodeGen::Context::getVoid()
{
    return &voidValue;
}
// other {{{1
IR::Value* CodeGen::Context::getGlobal(std::string const &name)
{
    auto v = globalSymbolTable.find(name);
    if (v == globalSymbolTable.end())
        return nullptr;
    return v->second;
}
void CodeGen::Context::addGlobal(std::string const &name, IR::Value *v)
{
    if (globalSymbolTable.find(name) != globalSymbolTable.end())
        reportAbortNoh(format("add duplicate global under name %", name));

    globalSymbolTable[name] = v;
}

