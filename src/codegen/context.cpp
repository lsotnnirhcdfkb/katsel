#include "codegenlocal.h"
#include "ir/unit.h"
#include "utils/format.h"
#include <iostream>
#include "ast/ast.h"

CodeGen::Context::Context(File const &file, CodeGen &cg): cg(cg), voidValue(getVoidType()) {}

// getting types {{{1 TODO: make a template function to loop through things and either make operator== = default for all types or use a lambda to compare them
#define GET_TYPE_DEF(type) NNPtr<IR::type> CodeGen::Context::get##type
#define LOOP_TYPES() for (std::unique_ptr<IR::Type> &_loopType : types)
#define CHECK_TYPE_TYPE(type) IR::type *_casted (dynamic_cast<IR::type*>(_loopType.get()));
#define CHECK_FIELD(field) (_casted->field == field)
#define CONSTRUCT_TYPE(type) std::unique_ptr<IR::type> _newType = std::make_unique<IR::type>
#define PUSH_RETURN(type) \
    NNPtr<IR::type> _newTypeR (_newType.get()); \
    types.push_back(std::move(_newType)); \
    return _newTypeR;

GET_TYPE_DEF(FloatType)(int size) {
    LOOP_TYPES() {
        CHECK_TYPE_TYPE(FloatType)
        if (_casted && CHECK_FIELD(size)) return _casted;
    }
    CONSTRUCT_TYPE(FloatType)(*this, cg.unit->implicitDeclAST.get(), size);
    PUSH_RETURN(FloatType)
}
GET_TYPE_DEF(IntType)(int size, bool isSigned) {
    LOOP_TYPES() {
        CHECK_TYPE_TYPE(IntType)
        if (_casted && CHECK_FIELD(size) && CHECK_FIELD(isSigned)) return _casted;
    }
    CONSTRUCT_TYPE(IntType)(*this, cg.unit->implicitDeclAST.get(), size, isSigned);
    PUSH_RETURN(IntType)
}
GET_TYPE_DEF(CharType)() {
    LOOP_TYPES() {
        CHECK_TYPE_TYPE(CharType)
        if (_casted) return _casted;
    }
    CONSTRUCT_TYPE(CharType)(*this, cg.unit->implicitDeclAST.get());
    PUSH_RETURN(CharType)
}
GET_TYPE_DEF(BoolType)() {
    LOOP_TYPES() {
        CHECK_TYPE_TYPE(BoolType)
        if (_casted) return _casted;
    }
    CONSTRUCT_TYPE(BoolType)(*this, cg.unit->implicitDeclAST.get());
    PUSH_RETURN(BoolType)
}
GET_TYPE_DEF(GenericIntType)() {
    LOOP_TYPES() {
        CHECK_TYPE_TYPE(GenericIntType)
        if (_casted) return _casted;
    }
    CONSTRUCT_TYPE(GenericIntType)(*this, cg.unit->implicitDeclAST.get());
    PUSH_RETURN(GenericIntType)
}
GET_TYPE_DEF(GenericFloatType)() {
    LOOP_TYPES() {
        CHECK_TYPE_TYPE(GenericFloatType)
        if (_casted) return _casted;
    }
    CONSTRUCT_TYPE(GenericFloatType)(*this, cg.unit->implicitDeclAST.get());
    PUSH_RETURN(GenericFloatType)
}
GET_TYPE_DEF(FunctionType)(NNPtr<IR::Type> ret, std::vector<NNPtr<IR::Type>> paramtys) {
    LOOP_TYPES() {
        CHECK_TYPE_TYPE(FunctionType)
        if (_casted && CHECK_FIELD(ret) && CHECK_FIELD(paramtys)) return _casted;
    }
    CONSTRUCT_TYPE(FunctionType)(*this, cg.unit->implicitDeclAST.get(), ret, paramtys);
    PUSH_RETURN(FunctionType)
}
GET_TYPE_DEF(VoidType)() {
    LOOP_TYPES() {
        CHECK_TYPE_TYPE(VoidType)
        if (_casted) return _casted;
    }
    CONSTRUCT_TYPE(VoidType)(*this, cg.unit->implicitDeclAST.get());
    PUSH_RETURN(VoidType)
}
GET_TYPE_DEF(PointerType)(bool mut, NNPtr<IR::Type> ty) {
    LOOP_TYPES() {
        CHECK_TYPE_TYPE(PointerType)
        if (_casted && CHECK_FIELD(mut) && CHECK_FIELD(ty)) return _casted;
    }
    CONSTRUCT_TYPE(PointerType)(*this, cg.unit->implicitDeclAST.get(), mut, ty);
    PUSH_RETURN(PointerType)
}
#undef GET_TYPE_DEF
#undef LOOP_TYPES
#undef CHECK_TYPE_TYPE
#undef CHECK_FIELD
#undef CONSTRUCT_TYPE
#undef PUSH_RETURN
// getting values {{{1
template <typename Ret, typename ... Args>
static NNPtr<Ret> getConstVal(std::vector<std::unique_ptr<IR::Value>> &constants, Args ...args) {
    std::unique_ptr<Ret> cv = std::make_unique<Ret>(args...);
    NNPtr<Ret> cvraw = cv.get();
    constants.push_back(std::move(cv));
    return cvraw;
}
NNPtr<IR::ConstFloat> CodeGen::Context::getConstFloat(NNPtr<IR::FloatType> ty, double value) {
    return getConstVal<IR::ConstFloat>(constants, ty, value);
}
NNPtr<IR::ConstInt> CodeGen::Context::getConstInt(NNPtr<IR::IntType> ty, uint64_t value) {
    return getConstVal<IR::ConstInt>(constants, ty, value);
}
NNPtr<IR::ConstFloat> CodeGen::Context::getConstFloat(NNPtr<IR::GenericFloatType> ty, double value) {
    return getConstVal<IR::ConstFloat>(constants, ty, value);
}
NNPtr<IR::ConstInt> CodeGen::Context::getConstInt(NNPtr<IR::GenericIntType> ty, uint64_t value) {
    return getConstVal<IR::ConstInt>(constants, ty, value);
}
NNPtr<IR::ConstChar> CodeGen::Context::getConstChar(uint8_t value) {
    return getConstVal<IR::ConstChar>(constants, getCharType(), value);
}
NNPtr<IR::ConstBool> CodeGen::Context::getConstBool(bool value) {
    return getConstVal<IR::ConstBool>(constants, getBoolType(), value);
}
NNPtr<IR::Void> CodeGen::Context::getVoid() {
    return &voidValue;
}
