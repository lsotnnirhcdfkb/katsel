#include "codegenlocal.h"
#include "ir/unit.h"
#include "utils/format.h"
#include <iostream>
#include "ast/ast.h"

CodeGen::Context::Context(File const &file, CodeGen &cg): cg(cg), void_value(get_void_type()) {}

// getting types {{{1 TODO: make a template function to loop through things and either make operator== = default for all types or use a lambda to compare them
#define GET_TYPE_DEF(ret, type) NNPtr<IR::ret> CodeGen::Context::get_##type
#define LOOP_TYPES() for (std::unique_ptr<IR::Type> &_loop_type : types)
#define CHECK_TYPE_TYPE(type) IR::type *_casted (dynamic_cast<IR::type*>(_loop_type.get()));
#define CHECK_FIELD(field) (_casted->field == field)
#define CONSTRUCT_TYPE(type) std::unique_ptr<IR::type> _new_type = std::make_unique<IR::type>
#define PUSH_RETURN(type) \
    NNPtr<IR::type> _new_type_r (_new_type.get()); \
    types.push_back(std::move(_new_type)); \
    return _new_type_r;

GET_TYPE_DEF(FloatType, float_type)(int size) {
    LOOP_TYPES() {
        CHECK_TYPE_TYPE(FloatType)
        if (_casted && CHECK_FIELD(size)) return _casted;
    }
    CONSTRUCT_TYPE(FloatType)(*this, cg.unit->implicit_decl_ast.get(), size);
    PUSH_RETURN(FloatType)
}
GET_TYPE_DEF(IntType, int_type)(int size, bool is_signed) {
    LOOP_TYPES() {
        CHECK_TYPE_TYPE(IntType)
        if (_casted && CHECK_FIELD(size) && CHECK_FIELD(is_signed)) return _casted;
    }
    CONSTRUCT_TYPE(IntType)(*this, cg.unit->implicit_decl_ast.get(), size, is_signed);
    PUSH_RETURN(IntType)
}
GET_TYPE_DEF(CharType, char_type)() {
    LOOP_TYPES() {
        CHECK_TYPE_TYPE(CharType)
        if (_casted) return _casted;
    }
    CONSTRUCT_TYPE(CharType)(*this, cg.unit->implicit_decl_ast.get());
    PUSH_RETURN(CharType)
}
GET_TYPE_DEF(BoolType, bool_type)() {
    LOOP_TYPES() {
        CHECK_TYPE_TYPE(BoolType)
        if (_casted) return _casted;
    }
    CONSTRUCT_TYPE(BoolType)(*this, cg.unit->implicit_decl_ast.get());
    PUSH_RETURN(BoolType)
}
GET_TYPE_DEF(GenericIntType, generic_int_type)() {
    LOOP_TYPES() {
        CHECK_TYPE_TYPE(GenericIntType)
        if (_casted) return _casted;
    }
    CONSTRUCT_TYPE(GenericIntType)(*this, cg.unit->implicit_decl_ast.get());
    PUSH_RETURN(GenericIntType)
}
GET_TYPE_DEF(GenericFloatType, generic_float_type)() {
    LOOP_TYPES() {
        CHECK_TYPE_TYPE(GenericFloatType)
        if (_casted) return _casted;
    }
    CONSTRUCT_TYPE(GenericFloatType)(*this, cg.unit->implicit_decl_ast.get());
    PUSH_RETURN(GenericFloatType)
}
GET_TYPE_DEF(FunctionType, function_type)(NNPtr<IR::Type> ret, std::vector<NNPtr<IR::Type>> paramtys) {
    LOOP_TYPES() {
        CHECK_TYPE_TYPE(FunctionType)
        if (_casted && CHECK_FIELD(ret) && CHECK_FIELD(paramtys)) return _casted;
    }
    CONSTRUCT_TYPE(FunctionType)(*this, cg.unit->implicit_decl_ast.get(), ret, paramtys);
    PUSH_RETURN(FunctionType)
}
GET_TYPE_DEF(VoidType, void_type)() {
    LOOP_TYPES() {
        CHECK_TYPE_TYPE(VoidType)
        if (_casted) return _casted;
    }
    CONSTRUCT_TYPE(VoidType)(*this, cg.unit->implicit_decl_ast.get());
    PUSH_RETURN(VoidType)
}
GET_TYPE_DEF(PointerType, pointer_type)(bool mut, NNPtr<IR::Type> ty) {
    LOOP_TYPES() {
        CHECK_TYPE_TYPE(PointerType)
        if (_casted && CHECK_FIELD(mut) && CHECK_FIELD(ty)) return _casted;
    }
    CONSTRUCT_TYPE(PointerType)(*this, cg.unit->implicit_decl_ast.get(), mut, ty);
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
static NNPtr<Ret> get_const_val(std::vector<std::unique_ptr<IR::Value>> &constants, Args ...args) {
    std::unique_ptr<Ret> cv = std::make_unique<Ret>(args...);
    NNPtr<Ret> cvraw = cv.get();
    constants.push_back(std::move(cv));
    return cvraw;
}
NNPtr<IR::ConstFloat> CodeGen::Context::get_const_float(NNPtr<IR::FloatType> ty, double value) {
    return get_const_val<IR::ConstFloat>(constants, ty, value);
}
NNPtr<IR::ConstInt> CodeGen::Context::get_const_int(NNPtr<IR::IntType> ty, uint64_t value) {
    return get_const_val<IR::ConstInt>(constants, ty, value);
}
NNPtr<IR::ConstFloat> CodeGen::Context::get_const_float(NNPtr<IR::GenericFloatType> ty, double value) {
    return get_const_val<IR::ConstFloat>(constants, ty, value);
}
NNPtr<IR::ConstInt> CodeGen::Context::get_const_int(NNPtr<IR::GenericIntType> ty, uint64_t value) {
    return get_const_val<IR::ConstInt>(constants, ty, value);
}
NNPtr<IR::ConstChar> CodeGen::Context::get_const_char(uint8_t value) {
    return get_const_val<IR::ConstChar>(constants, get_char_type(), value);
}
NNPtr<IR::ConstBool> CodeGen::Context::get_const_bool(bool value) {
    return get_const_val<IR::ConstBool>(constants, get_bool_type(), value);
}
NNPtr<IR::Void> CodeGen::Context::get_void() {
    return &void_value;
}
