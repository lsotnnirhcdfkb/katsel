#include "codegenlocal.h"
#include "ir/unit.h"
#include "utils/format.h"
#include <iostream>
#include "ast/ast.h"

CodeGen::Context::Context(File const &file): implicit_decl_ast(std::make_unique<ASTNS::ImplicitDecl>(file, Maybe<Span const>(), 0)), void_value(get_void_type()) {}

// getting types {{{1 TODO: make a template function to loop through things and either make operator== = default for all types or use a lambda to compare them
#define CHECK_FIELD(field) (casted->field == field)

#define GET_TYPE_DEF(get_ret, method_name, params, fields, args) \
    IR::get_ret& CodeGen::Context::get_##method_name params /* params are already wrapped in () */ { \
        for (std::unique_ptr<IR::Type> &loop_type : types) { \
            IR::get_ret *casted (dynamic_cast<IR::get_ret*>(loop_type.get())); \
            /* fields in wrappd in (), which makes this a macro invocation */ \
            if (casted && fields ) return *casted; \
        } \
        auto new_type = std::make_unique<IR::get_ret> args; /* args are also already wrapped in () */ ; \
        IR::get_ret &new_type_r (*new_type); \
        types.push_back(std::move(new_type)); \
        return new_type_r; \
    }

GET_TYPE_DEF(FloatType, float_type,
        (int size),
        (CHECK_FIELD(size)),
        (*this, *cg.unit->implicit_decl_ast, size))

GET_TYPE_DEF(IntType, int_type,
        (int size, bool is_signed),
        (CHECK_FIELD(size) && CHECK_FIELD(is_signed)),
        (*this, *cg.unit->implicit_decl_ast, size, is_signed))

GET_TYPE_DEF(CharType, char_type,
        (),
        true,
        (*this, *cg.unit->implicit_decl_ast))

GET_TYPE_DEF(BoolType, bool_type,
        (),
        true,
        (*this, *cg.unit->implicit_decl_ast))

GET_TYPE_DEF(GenericIntType, generic_int_type,
        (),
        true,
        (*this, *cg.unit->implicit_decl_ast))

GET_TYPE_DEF(GenericFloatType, generic_float_type,
        (),
        true,
        (*this, *cg.unit->implicit_decl_ast))

GET_TYPE_DEF(FunctionType, function_type,
        (IR::Type const &ret, std::vector<NNPtr<IR::Type const>> paramtys),
        (&ret == &*casted->ret  && CHECK_FIELD(paramtys)),
        (*this, *cg.unit->implicit_decl_ast, ret, paramtys))

GET_TYPE_DEF(VoidType, void_type,
        (),
        true,
        (*this, *cg.unit->implicit_decl_ast))

GET_TYPE_DEF(PointerType, pointer_type,
        (bool mut, IR::Type const &ty),
        (CHECK_FIELD(mut) && casted->ty.as_raw() == &ty),
        (*this, *cg.unit->implicit_decl_ast, mut, ty))

#undef GET_TYPE_DEF
#undef CHECK_FIELD
// getting values {{{1
template <typename Ret, typename ... Args>
static Ret& get_const_val(std::vector<std::unique_ptr<IR::Value>> &constants, Args && ...args) {
    std::unique_ptr<Ret> cv = std::make_unique<Ret>(std::forward<Args>(args)...);
    Ret& cvraw = *cv;
    constants.push_back(std::move(cv));
    return cvraw;
}

IR::ConstFloat& CodeGen::Context::get_const_float(IR::FloatType &ty, double value) {
    return get_const_val<IR::ConstFloat>(constants, ty, value);
}
IR::ConstInt& CodeGen::Context::get_const_int(IR::IntType& ty, uint64_t value) {
    return get_const_val<IR::ConstInt>(constants, ty, value);
}
IR::ConstFloat& CodeGen::Context::get_const_float(IR::GenericFloatType &ty, double value) {
    return get_const_val<IR::ConstFloat>(constants, ty, value);
}
IR::ConstInt& CodeGen::Context::get_const_int(IR::GenericIntType &ty, uint64_t value) {
    return get_const_val<IR::ConstInt>(constants, ty, value);
}
IR::ConstChar& CodeGen::Context::get_const_char(uint8_t value) {
    return get_const_val<IR::ConstChar>(constants, get_char_type(), value);
}
IR::ConstBool& CodeGen::Context::get_const_bool(bool value) {
    return get_const_val<IR::ConstBool>(constants, get_bool_type(), value);
}
IR::Void& CodeGen::Context::get_void() {
    return void_value;
}
