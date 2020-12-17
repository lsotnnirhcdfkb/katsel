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
IR::ConstFloat* CodeGen::Context::getConstFloat(IR::FloatType *ty, double value)
{
    std::unique_ptr<IR::ConstFloat> ci = std::make_unique<IR::ConstFloat>(ty, value);
    IR::ConstFloat *ciraw = ci.get();
    constants.push_back(std::move(ci));
    return ciraw;
}
IR::ConstInt* CodeGen::Context::getConstInt(IR::IntType *ty, uint64_t value)
{
    std::unique_ptr<IR::ConstInt> ci = std::make_unique<IR::ConstInt>(ty, value);
    IR::ConstInt *ciraw = ci.get();
    constants.push_back(std::move(ci));
    return ciraw;
}
IR::ConstChar* CodeGen::Context::getConstChar(uint8_t value)
{
    std::unique_ptr<IR::ConstChar> ci = std::make_unique<IR::ConstChar>(getCharType(), value);
    IR::ConstChar *ciraw = ci.get();
    constants.push_back(std::move(ci));
    return ciraw;
}
IR::ConstBool* CodeGen::Context::getConstBool(bool value)
{
    std::unique_ptr<IR::ConstBool> ci = std::make_unique<IR::ConstBool>(getBoolType(), value);
    IR::ConstBool *ciraw = ci.get();
    constants.push_back(std::move(ci));
    return ciraw;
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

