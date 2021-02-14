#include "ir/type.h"
#include "ir/function.h"
#include "ir/block.h"
#include "ir/instruction.h"
#include "message/errmsgs.h"
#include "utils/format.h"
#include "utils/assert.h"

#include "codegen/codegen.h"
#include "codegen/context.h"

#include "llvm/IR/DerivedTypes.h"

#define BIN_OP_ARGS Codegen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, Located<ASTNS::BinaryOperator> op, Located<NNPtr<IR::Value>> l, Located<NNPtr<IR::Value>> r, ASTNS::AST const &ast
#define UNARY_OP_ARGS Codegen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, Located<ASTNS::UnaryOperator> op, Located<NNPtr<IR::Value>> v, ASTNS::AST const &ast
// static functions {{{1
#define SUPPORT_OPERATOR_BASIC(op, instr, out_ty) \
    case ASTNS::BinaryOperator::op: { \
        return Located<NNPtr<IR::Value>> { ast, cur_block->add<IR::Instrs::instr>(l, r) }; \
    }
// float/int operations for reuse between generic float/int and concrete float/int types {{{2
static Maybe<Located<NNPtr<IR::Value>>> float_bin_op(BIN_OP_ARGS) {
    l = r.value->type().impl_cast(cgc, fun, cur_block, l);
    r = l.value->type().impl_cast(cgc, fun, cur_block, r);

    if (&l.value->type() != &r.value->type()) {
        ERR_CONFLICT_TYS_BINARY_OP(l, r, op.span);
        return Maybe<Located<NNPtr<IR::Value>>>();
    }

    switch (op.value) {
        SUPPORT_OPERATOR_BASIC(PLUS, FAdd, l.value->type())
        SUPPORT_OPERATOR_BASIC(MINUS, FSub, l.value->type())
        SUPPORT_OPERATOR_BASIC(STAR, FMult, l.value->type())
        SUPPORT_OPERATOR_BASIC(SLASH, FDiv, l.value->type())
        SUPPORT_OPERATOR_BASIC(PERCENT, FMod, l.value->type())
        SUPPORT_OPERATOR_BASIC(GREATER, FCmpGT, cgc.get_bool_type())
        SUPPORT_OPERATOR_BASIC(LESS, FCmpLT, cgc.get_bool_type())
        SUPPORT_OPERATOR_BASIC(GREATEREQUAL, FCmpGE, cgc.get_bool_type())
        SUPPORT_OPERATOR_BASIC(LESSEQUAL, FCmpLE, cgc.get_bool_type())
        SUPPORT_OPERATOR_BASIC(DOUBLEEQUAL, FCmpEQ, cgc.get_bool_type())
        SUPPORT_OPERATOR_BASIC(BANGEQUAL, FCmpNE, cgc.get_bool_type())

        default:
            ERR_LHS_UNSUPPORTED_OP(l, op.span);
            return Maybe<Located<NNPtr<IR::Value>>>();
    }
}
static Maybe<Located<NNPtr<IR::Value>>> float_unary_op(UNARY_OP_ARGS) {
    switch (op.value) {
        case ASTNS::UnaryOperator::MINUS: {
            return Located<NNPtr<IR::Value>> { ast, cur_block->add<IR::Instrs::FNeg>(v) };
        }

        default:
            ERR_UNARY_UNSUPPORTED_OP(v, op);
            return Maybe<Located<NNPtr<IR::Value>>>();
    }
}
static Maybe<Located<NNPtr<IR::Value>>> int_bin_op(BIN_OP_ARGS) {
    l = r.value->type().impl_cast(cgc, fun, cur_block, l);
    r = l.value->type().impl_cast(cgc, fun, cur_block, r);

    if (&l.value->type() != &r.value->type()) {
        ERR_CONFLICT_TYS_BINARY_OP(l, r, op.span);
        return Maybe<Located<NNPtr<IR::Value>>>();
    }

    switch (op.value) {
        SUPPORT_OPERATOR_BASIC(PLUS, IAdd, l.value->type())
        SUPPORT_OPERATOR_BASIC(MINUS, ISub, l.value->type())
        SUPPORT_OPERATOR_BASIC(STAR, IMult, l.value->type())
        SUPPORT_OPERATOR_BASIC(SLASH, IDiv, l.value->type())
        SUPPORT_OPERATOR_BASIC(PERCENT, IMod, l.value->type())
        SUPPORT_OPERATOR_BASIC(GREATER, ICmpGT, cgc.get_bool_type())
        SUPPORT_OPERATOR_BASIC(LESS, ICmpLT, cgc.get_bool_type())
        SUPPORT_OPERATOR_BASIC(GREATEREQUAL, ICmpGE, cgc.get_bool_type())
        SUPPORT_OPERATOR_BASIC(LESSEQUAL, ICmpLE, cgc.get_bool_type())
        SUPPORT_OPERATOR_BASIC(DOUBLEEQUAL, ICmpEQ, cgc.get_bool_type())
        SUPPORT_OPERATOR_BASIC(BANGEQUAL, ICmpNE, cgc.get_bool_type())
        SUPPORT_OPERATOR_BASIC(AMPER, BitAnd, l.value->type())
        SUPPORT_OPERATOR_BASIC(PIPE, BitOr, l.value->type())
        SUPPORT_OPERATOR_BASIC(CARET, BitXor, l.value->type())
        SUPPORT_OPERATOR_BASIC(DOUBLEGREATER, ShiftR, l.value->type())
        SUPPORT_OPERATOR_BASIC(DOUBLELESS, ShiftL, l.value->type())

        default:
            ERR_LHS_UNSUPPORTED_OP(l, op.span);
            return Maybe<Located<NNPtr<IR::Value>>>();
    }
}
static Maybe<Located<NNPtr<IR::Value>>> int_unary_op(UNARY_OP_ARGS) {
    switch (op.value) {
        case ASTNS::UnaryOperator::TILDE: {
            return Located<NNPtr<IR::Value>> { ast, cur_block->add<IR::Instrs::BitNot>(v) };
        }

        case ASTNS::UnaryOperator::MINUS: {
            return Located<NNPtr<IR::Value>> { ast, cur_block->add<IR::Instrs::INeg>(v) };
        }

        default:
            ERR_UNARY_UNSUPPORTED_OP(v, op);
            return Maybe<Located<NNPtr<IR::Value>>>();
    }
}
// Float and Int {{{1
// Float {{{2
IR::FloatType::FloatType(Codegen::Context &context, ASTNS::AST const &decl_ast, int size): Type(context), size(size), _decl_ast(decl_ast) {ASSERT(size == 32 || size == 64)}
ASTNS::AST const &IR::FloatType::decl_ast() const {
    return *_decl_ast;
}

llvm::Type& IR::FloatType::to_llvm_type(llvm::LLVMContext &con) const {
    if (size == 32)
        return *llvm::Type::getFloatTy(con);
    else if (size == 64)
        return *llvm::Type::getDoubleTy(con);
    else
        report_abort_noh(format("FloatType::to_llvm_type: size = {}", size));
}
std::string IR::FloatType::name() const {
    if (size == 32)
        return "float";
    else if (size == 64)
        return "double";
    else
        report_abort_noh(format("FloatType::name: size = {}", size));
}
Maybe<Located<NNPtr<IR::Value>>> IR::FloatType::bin_op(BIN_OP_ARGS) const {
    ASSERT(&l.value->type() == this);
    return float_bin_op(cgc, fun, cur_block, op, l, r, ast);
}
Maybe<Located<NNPtr<IR::Value>>> IR::FloatType::unary_op(Codegen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, Located<ASTNS::UnaryOperator> op, Located<NNPtr<IR::Value>> v, ASTNS::AST const &ast) const {
    ASSERT(&v.value->type() == this);
    return float_unary_op(cgc, fun, cur_block, op, v, ast);
}
Maybe<Located<NNPtr<IR::Value>>> IR::FloatType::cast_from(Codegen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, Located<NNPtr<IR::Value>> v, ASTNS::AST const &ast) const {
    if (&v.value->type() == this)
        return Located<NNPtr<IR::Value>> { ast, v.value };

    v = impl_cast(cgc, fun, cur_block, v); // try implicit cast to self

    if (&v.value->type() == this)
        return Located<NNPtr<IR::Value>> { ast, v.value };

    IntType const *sty (dynamic_cast<IntType const *>(&v.value->type()));
    if (sty) {
        return Located<NNPtr<IR::Value>> { ast, cur_block->add<IR::Instrs::IntToFloat>(v, this) };
    } else if (dynamic_cast<FloatType const *>(&v.value->type())) {
        return Located<NNPtr<IR::Value>> { ast, cur_block->add<IR::Instrs::FloatToFloat>(v, this) };
    } else {
        ERR_INVALID_CAST(ast, v, *this);
        return Maybe<Located<NNPtr<IR::Value>>>();
    }
}
Located<NNPtr<IR::Value>> IR::FloatType::impl_cast(Codegen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, Located<NNPtr<IR::Value>> v) const {
    if (dynamic_cast<GenericFloatType const *>(&v.value->type())) {
        return Located<NNPtr<IR::Value>> { v.span, cur_block->add<IR::Instrs::FloatToFloat>(v, this) };
    }

    return v;
}
// Int {{{2
IR::IntType::IntType(Codegen::Context &context, ASTNS::AST const &decl_ast, int size, bool is_signed): Type(context), size(size), is_signed(is_signed), _decl_ast(decl_ast) {ASSERT(size == 1 || size == 8 || size == 16 || size == 32 || size == 64)}
ASTNS::AST const &IR::IntType::decl_ast() const {
    return *_decl_ast;
}

llvm::Type& IR::IntType::to_llvm_type(llvm::LLVMContext &con) const {
    return *llvm::IntegerType::get(con, size);
}
std::string IR::IntType::name() const {
    return format("{}int{}", is_signed ? 's' : 'u', size);
}
Maybe<Located<NNPtr<IR::Value>>> IR::IntType::bin_op(BIN_OP_ARGS) const {
    ASSERT(&l.value->type() == this)
    return int_bin_op(cgc, fun, cur_block, op, l, r, ast);
}
Maybe<Located<NNPtr<IR::Value>>> IR::IntType::unary_op(Codegen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, Located<ASTNS::UnaryOperator> op, Located<NNPtr<IR::Value>> v, ASTNS::AST const &ast) const {
    ASSERT(&v.value->type() == this)
    return int_unary_op(cgc, fun, cur_block, op, v, ast);
}
Maybe<Located<NNPtr<IR::Value>>> IR::IntType::cast_from(Codegen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, Located<NNPtr<IR::Value>> v, ASTNS::AST const &ast) const {
    if (&v.value->type() == this)
        return Located<NNPtr<IR::Value>> { ast, v.value };

    v = impl_cast(cgc, fun, cur_block, v); // try implicit cast to self

    if (&v.value->type() == this)
        return Located<NNPtr<IR::Value>> { ast, v.value };

    IntType const *sty_int (dynamic_cast<IntType const *> (&v.value->type()));
    FloatType const *sty_float (dynamic_cast<FloatType const *> (&v.value->type()));
    CharType const *sty_char (dynamic_cast<CharType const *> (&v.value->type()));
    BoolType const *sty_bool (dynamic_cast<BoolType const *> (&v.value->type()));

    if (sty_int || sty_char || sty_bool) {
        if (sty_char) {
            IR::IntType &newt (cgc.get_int_type(8, false));
            sty_int = &newt;
            sty_char = nullptr;

            v = Located<NNPtr<IR::Value>> { v.span, cur_block->add<IR::Instrs::NoOpCast>(v, newt) };

        } else if (sty_bool) {
            IR::IntType &newt (cgc.get_int_type(1, false));
            sty_int = &newt;
            sty_bool = nullptr;

            v = Located<NNPtr<IR::Value>> { v.span, cur_block->add<IR::Instrs::NoOpCast>(v, newt) };
        }

        return Located<NNPtr<IR::Value>> { ast, cur_block->add<IR::Instrs::IntToInt>(v, this) };
    } else if (sty_float) {
        return Located<NNPtr<IR::Value>> { ast, cur_block->add<IR::Instrs::FloatToInt>(v, this) };
    } else {
        ERR_INVALID_CAST(ast, v, *this);
        return Maybe<Located<NNPtr<IR::Value>>>();
    }
}
Located<NNPtr<IR::Value>> IR::IntType::impl_cast(Codegen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, Located<NNPtr<IR::Value>> v) const {
    if (dynamic_cast<GenericIntType const *>(&v.value->type())) {
        return Located<NNPtr<IR::Value>> { v.span, cur_block->add<IR::Instrs::IntToInt>(v, this) };
    }

    return v;
}
// Generic types {{{2
// GenericInt {{{2
IR::GenericIntType::GenericIntType(Codegen::Context &context, ASTNS::AST const &decl_ast): Type(context), _decl_ast(decl_ast) {}
ASTNS::AST const &IR::GenericIntType::decl_ast() const {
    return *_decl_ast;
}

std::string IR::GenericIntType::name() const {
    return "<integer>";
}
Maybe<Located<NNPtr<IR::Value>>> IR::GenericIntType::bin_op(BIN_OP_ARGS) const {
    ASSERT(&l.value->type() == this);
    return int_bin_op(cgc, fun, cur_block, op, l, r, ast);
}
Maybe<Located<NNPtr<IR::Value>>> IR::GenericIntType::unary_op(UNARY_OP_ARGS) const {
    ASSERT(&v.value->type() == this);
    return int_unary_op(cgc, fun, cur_block, op, v, ast);
}
Maybe<Located<NNPtr<IR::Value>>> IR::GenericIntType::cast_from(Codegen::Context &, IR::Function &, NNPtr<IR::Block> &, Located<NNPtr<IR::Value>> v, ASTNS::AST const &ast) const {
    ERR_INVALID_CAST(ast, v, *this);
    return Maybe<Located<NNPtr<Value>>>();
}
llvm::Type& IR::GenericIntType::to_llvm_type(llvm::LLVMContext &con) const {
    return *llvm::Type::getInt32Ty(con);
}
Located<NNPtr<IR::Value>> IR::GenericIntType::impl_cast(Codegen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, Located<NNPtr<IR::Value>> v) const {
    return v;
}
// GenericFloat {{{2
IR::GenericFloatType::GenericFloatType(Codegen::Context &context, ASTNS::AST const &decl_ast): Type(context), _decl_ast(decl_ast) {}
ASTNS::AST const &IR::GenericFloatType::decl_ast() const {
    return *_decl_ast;
}

std::string IR::GenericFloatType::name() const {
    return "<float>";
}
Maybe<Located<NNPtr<IR::Value>>> IR::GenericFloatType::bin_op(BIN_OP_ARGS) const {
    ASSERT(&l.value->type() == this);
    return float_bin_op(cgc, fun, cur_block, op, l, r, ast);
}
Maybe<Located<NNPtr<IR::Value>>> IR::GenericFloatType::unary_op(UNARY_OP_ARGS) const {
    ASSERT(&v.value->type() == this);
    return float_unary_op(cgc, fun, cur_block, op, v, ast);
}
Maybe<Located<NNPtr<IR::Value>>> IR::GenericFloatType::cast_from(Codegen::Context &, IR::Function &, NNPtr<IR::Block> &, Located<NNPtr<IR::Value>> v, ASTNS::AST const &ast) const {
    ERR_INVALID_CAST(ast, v, *this);
    return Maybe<Located<NNPtr<Value>>>();
}
llvm::Type& IR::GenericFloatType::to_llvm_type(llvm::LLVMContext &con) const {
    return *llvm::Type::getFloatTy(con);
}

Located<NNPtr<IR::Value>> IR::GenericFloatType::impl_cast(Codegen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, Located<NNPtr<IR::Value>> v) const {
    return v;
}
// Char {{{1
IR::CharType::CharType(Codegen::Context &context, ASTNS::AST const &decl_ast): Type(context), _decl_ast(decl_ast) {}
ASTNS::AST const &IR::CharType::decl_ast() const {
    return *_decl_ast;
}

llvm::Type& IR::CharType::to_llvm_type(llvm::LLVMContext &con) const {
    return *llvm::Type::getInt8Ty(con);
}
std::string IR::CharType::name() const {
    return "char";
}
Maybe<Located<NNPtr<IR::Value>>> IR::CharType::bin_op(BIN_OP_ARGS) const {
    ASSERT(&l.value->type() == this)

    l = r.value->type().impl_cast(cgc, fun, cur_block, l);
    r = l.value->type().impl_cast(cgc, fun, cur_block, r);
    if (&l.value->type() != &r.value->type()) {
        ERR_CONFLICT_TYS_BINARY_OP(l, r, op.span);
        return Maybe<Located<NNPtr<IR::Value>>>();
    }

    switch (op.value) {
        SUPPORT_OPERATOR_BASIC(GREATER, ICmpGT, cgc.get_bool_type())
        SUPPORT_OPERATOR_BASIC(LESS, ICmpLT, cgc.get_bool_type())
        SUPPORT_OPERATOR_BASIC(GREATEREQUAL, ICmpGE, cgc.get_bool_type())
        SUPPORT_OPERATOR_BASIC(LESSEQUAL, ICmpLE, cgc.get_bool_type())
        SUPPORT_OPERATOR_BASIC(DOUBLEEQUAL, ICmpEQ, cgc.get_bool_type())
        SUPPORT_OPERATOR_BASIC(BANGEQUAL, ICmpNE, cgc.get_bool_type())

        default:
            ERR_LHS_UNSUPPORTED_OP(l, op.span);
            return Maybe<Located<NNPtr<Value>>>();
    }
}
Maybe<Located<NNPtr<IR::Value>>> IR::CharType::unary_op(Codegen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, Located<ASTNS::UnaryOperator> op, Located<NNPtr<IR::Value>> v, ASTNS::AST const &ast) const {
    ASSERT(&v.value->type() == this)

    ERR_UNARY_UNSUPPORTED_OP(v, op);
    return Maybe<Located<NNPtr<Value>>>();
}
Maybe<Located<NNPtr<IR::Value>>> IR::CharType::cast_from(Codegen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, Located<NNPtr<IR::Value>> v, ASTNS::AST const &ast) const {
    if (&v.value->type() == this)
        return Located<NNPtr<IR::Value>> { ast, v.value };

    IntType const *sty (dynamic_cast<IntType const *>(&v.value->type()));
    if (!sty) {
        ERR_INVALID_CAST(ast, v, *this);
        return Maybe<Located<NNPtr<IR::Value>>>();
    }

    IR::IntType const &char_as_int_type (cgc.get_int_type(8, false));

    IR::Instrs::IntToInt &as_int = cur_block->add<IR::Instrs::IntToInt>(v, char_as_int_type);
    return Located<NNPtr<IR::Value>> { ast, cur_block->add<IR::Instrs::NoOpCast>(Located<NNPtr<IR::Value>> { v.span, as_int }, this) };
}
Located<NNPtr<IR::Value>> IR::CharType::impl_cast(Codegen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, Located<NNPtr<IR::Value>> v) const {
    return v;
}
// Bool {{{1
IR::BoolType::BoolType(Codegen::Context &context, ASTNS::AST const &decl_ast): Type(context), _decl_ast(decl_ast) {}
ASTNS::AST const &IR::BoolType::decl_ast() const {
    return *_decl_ast;
}

llvm::Type& IR::BoolType::to_llvm_type(llvm::LLVMContext &con) const {
    return *llvm::Type::getInt1Ty(con);
}
std::string IR::BoolType::name() const {
    return "bool";
}
Maybe<Located<NNPtr<IR::Value>>> IR::BoolType::bin_op(Codegen::Context &cgc, Function &fun, NNPtr<Block> &cur_block, Located<ASTNS::BinaryOperator> op, Located<NNPtr<IR::Value>> l, Located<NNPtr<IR::Value>> r, ASTNS::AST const &ast) const {
    ASSERT(&l.value->type() == this)

    l = r.value->type().impl_cast(cgc, fun, cur_block, l);
    r = l.value->type().impl_cast(cgc, fun, cur_block, r);
    if (&l.value->type() != &r.value->type()) {
        ERR_CONFLICT_TYS_BINARY_OP(l, r, op.span);
        return Maybe<Located<NNPtr<IR::Value>>>();
    }

    switch (op.value) {
        SUPPORT_OPERATOR_BASIC(AMPER, BitAnd, l.value->type())
        SUPPORT_OPERATOR_BASIC(PIPE, BitOr, l.value->type())
        SUPPORT_OPERATOR_BASIC(CARET, BitXor, l.value->type())
        SUPPORT_OPERATOR_BASIC(DOUBLEEQUAL, ICmpEQ, cgc.get_bool_type())
        SUPPORT_OPERATOR_BASIC(BANGEQUAL, ICmpNE, cgc.get_bool_type())

        default:
            ERR_LHS_UNSUPPORTED_OP(l, op.span);
            return Maybe<Located<NNPtr<Value>>>();
    }
}
Maybe<Located<NNPtr<IR::Value>>> IR::BoolType::unary_op(Codegen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, Located<ASTNS::UnaryOperator> op, Located<NNPtr<IR::Value>> v, ASTNS::AST const &ast) const {
    ASSERT(&v.value->type() == this)

    switch (op.value) {
        case ASTNS::UnaryOperator::BANG: {
            return Located<NNPtr<IR::Value>> { ast, cur_block->add<IR::Instrs::Not>(v) };
         }

        default:
            ERR_UNARY_UNSUPPORTED_OP(v, op);
            return Maybe<Located<NNPtr<Value>>>();
    }
}
Maybe<Located<NNPtr<IR::Value>>> IR::BoolType::cast_from(Codegen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, Located<NNPtr<IR::Value>> v, ASTNS::AST const &ast) const {
    if (&v.value->type() == this)
        return Located<NNPtr<IR::Value>> { ast, v.value };

    IntType const *sty (dynamic_cast<IntType const *>(&v.value->type()));
    if (!sty) {
        ERR_INVALID_CAST(ast, v, *this);
        return Maybe<Located<NNPtr<IR::Value>>>();
    }

    IR::IntType const &bool_as_int_type (cgc.get_int_type(1, false));

    IR::Instrs::IntToInt &as_int = cur_block->add<IR::Instrs::IntToInt>(v, bool_as_int_type);

    return Located<NNPtr<IR::Value>> { ast, cur_block->add<IR::Instrs::NoOpCast>(Located<NNPtr<IR::Value>> { v.span, as_int }, *this) };
}
Located<NNPtr<IR::Value>> IR::BoolType::impl_cast(Codegen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, Located<NNPtr<IR::Value>> v) const {
    return v;
}
// Deriving {{{1
DERIVE_DECLSYMBOL_ITEMS_IMPL(IR::FloatType)
DERIVE_DECLSYMBOL_ITEMS_IMPL(IR::IntType)
DERIVE_DECLSYMBOL_ITEMS_IMPL(IR::GenericIntType)
DERIVE_DECLSYMBOL_ITEMS_IMPL(IR::GenericFloatType)
DERIVE_DECLSYMBOL_ITEMS_IMPL(IR::CharType)
DERIVE_DECLSYMBOL_ITEMS_IMPL(IR::BoolType)

DERIVE_TYPE_METHOD_TABLE_IMPL(IR::FloatType)
DERIVE_TYPE_METHOD_TABLE_IMPL(IR::IntType)
DERIVE_TYPE_METHOD_TABLE_IMPL(IR::GenericIntType)
DERIVE_TYPE_METHOD_TABLE_IMPL(IR::GenericFloatType)
DERIVE_TYPE_METHOD_TABLE_IMPL(IR::CharType)
DERIVE_TYPE_METHOD_TABLE_IMPL(IR::BoolType)

DERIVE_TYPE_NO_FIELDS(IR::FloatType)
DERIVE_TYPE_NO_FIELDS(IR::IntType)
DERIVE_TYPE_NO_FIELDS(IR::GenericIntType)
DERIVE_TYPE_NO_FIELDS(IR::GenericFloatType)
DERIVE_TYPE_NO_FIELDS(IR::CharType)
DERIVE_TYPE_NO_FIELDS(IR::BoolType)
