#include "ir/type.h"
#include "ir/block.h"
#include "ir/instruction.h"
#include "message/errmsgs.h"
#include "utils/format.h"
#include "utils/assert.h"

#include "codegen/codegen.h"
#include "codegen/context.h"

#include "llvm/IR/DerivedTypes.h"

#define BIN_OP_ARGS CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, Located<ASTNS::BinaryOperator> op, IR::ASTValue l, IR::ASTValue r, ASTNS::AST const &ast
#define UNARY_OP_ARGS CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, Located<ASTNS::UnaryOperator> op, IR::ASTValue v, ASTNS::AST const &ast
// static functions {{{1
#define SUPPORT_OPERATOR_BASIC(op, instr) case ASTNS::BinaryOperator::op: return IR::ASTValue(cur_block->add<IR::Instrs::instr>(l, r), ast);
// float/int operations for reuse between generic float/int and concrete float/int types {{{2
static Maybe<IR::ASTValue> float_bin_op(BIN_OP_ARGS) {
    l = r.type().impl_cast(cgc, fun, cur_block, l);
    r = l.type().impl_cast(cgc, fun, cur_block, r);

    if (&l.type() != &r.type()) {
        ERR_CONFLICT_TYS_BINARY_OP(l, r, op);
        return Maybe<IR::ASTValue>();
    }

    switch (op.value) {
        SUPPORT_OPERATOR_BASIC(PLUS, FAdd)
        SUPPORT_OPERATOR_BASIC(MINUS, FSub)
        SUPPORT_OPERATOR_BASIC(STAR, FMult)
        SUPPORT_OPERATOR_BASIC(SLASH, FDiv)
        SUPPORT_OPERATOR_BASIC(PERCENT, FMod)
        SUPPORT_OPERATOR_BASIC(GREATER, FCmpGT)
        SUPPORT_OPERATOR_BASIC(LESS, FCmpLT)
        SUPPORT_OPERATOR_BASIC(GREATEREQUAL, FCmpGE)
        SUPPORT_OPERATOR_BASIC(LESSEQUAL, FCmpLE)
        SUPPORT_OPERATOR_BASIC(DOUBLEEQUAL, FCmpEQ)
        SUPPORT_OPERATOR_BASIC(BANGEQUAL, FCmpNE)

        default:
            ERR_LHS_UNSUPPORTED_OP(l, op);
            return Maybe<IR::ASTValue>();
    }
}
static Maybe<IR::ASTValue> float_unary_op(UNARY_OP_ARGS) {
    switch (op.value) {
        case ASTNS::UnaryOperator::MINUS:
            return IR::ASTValue(cur_block->add<IR::Instrs::FNeg>(v), ast);

        default:
            ERR_UNARY_UNSUPPORTED_OP(v, op);
            return Maybe<IR::ASTValue>();
    }
}
static Maybe<IR::ASTValue> int_bin_op(BIN_OP_ARGS) {
    l = r.type().impl_cast(cgc, fun, cur_block, l);
    r = l.type().impl_cast(cgc, fun, cur_block, r);

    if (&l.type() != &r.type()) {
        ERR_CONFLICT_TYS_BINARY_OP(l, r, op);
        return Maybe<IR::ASTValue>();
    }

    switch (op.value) {
        SUPPORT_OPERATOR_BASIC(PLUS, IAdd)
        SUPPORT_OPERATOR_BASIC(MINUS, ISub)
        SUPPORT_OPERATOR_BASIC(STAR, IMult)
        SUPPORT_OPERATOR_BASIC(SLASH, IDiv)
        SUPPORT_OPERATOR_BASIC(PERCENT, IMod)
        SUPPORT_OPERATOR_BASIC(GREATER, ICmpGT)
        SUPPORT_OPERATOR_BASIC(LESS, ICmpLT)
        SUPPORT_OPERATOR_BASIC(GREATEREQUAL, ICmpGE)
        SUPPORT_OPERATOR_BASIC(LESSEQUAL, ICmpLE)
        SUPPORT_OPERATOR_BASIC(DOUBLEEQUAL, ICmpEQ)
        SUPPORT_OPERATOR_BASIC(BANGEQUAL, ICmpNE)
        SUPPORT_OPERATOR_BASIC(AMPER, BitAnd)
        SUPPORT_OPERATOR_BASIC(PIPE, BitOr)
        SUPPORT_OPERATOR_BASIC(CARET, BitXor)
        SUPPORT_OPERATOR_BASIC(DOUBLEGREATER, ShiftR)
        SUPPORT_OPERATOR_BASIC(DOUBLELESS, ShiftL)

        default:
            ERR_LHS_UNSUPPORTED_OP(l, op);
            return Maybe<IR::ASTValue>();
    }
}
static Maybe<IR::ASTValue> int_unary_op(UNARY_OP_ARGS) {
    switch (op.value) {
        case ASTNS::UnaryOperator::TILDE:
            return IR::ASTValue(cur_block->add<IR::Instrs::BitNot>(v), ast);

        case ASTNS::UnaryOperator::MINUS:
            return IR::ASTValue(cur_block->add<IR::Instrs::INeg>(v), ast);

        default:
            ERR_UNARY_UNSUPPORTED_OP(v, op);
            return Maybe<IR::ASTValue>();
    }
}
// Float and Int {{{1
// Float {{{2
IR::FloatType::FloatType(CodeGen::Context &context, ASTNS::AST const &decl_ast, int size): Type(context), size(size), _decl_ast(decl_ast) {ASSERT(size == 32 || size == 64)}
ASTNS::AST const &IR::FloatType::decl_ast() const {
    return *_decl_ast;
}

llvm::Type& IR::FloatType::to_llvmtype(llvm::LLVMContext &con) const {
    if (size == 32)
        return *llvm::Type::getFloatTy(con);
    else if (size == 64)
        return *llvm::Type::getDoubleTy(con);
    else
        report_abort_noh(format("FloatType::to_llvmtype: size = {}", size));
}
std::string IR::FloatType::name() const {
    if (size == 32)
        return "float";
    else if (size == 64)
        return "double";
    else
        report_abort_noh(format("FloatType::name: size = {}", size));
}
Maybe<IR::ASTValue> IR::FloatType::bin_op(BIN_OP_ARGS) const {
    ASSERT(&l.type() == this);
    return float_bin_op(cgc, fun, cur_block, op, l, r, ast);
}
Maybe<IR::ASTValue> IR::FloatType::unary_op(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, Located<ASTNS::UnaryOperator> op, IR::ASTValue v, ASTNS::AST const &ast) const {
    ASSERT(&v.type() == this);
    return float_unary_op(cgc, fun, cur_block, op, v, ast);
}
Maybe<IR::ASTValue> IR::FloatType::cast_from(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, IR::ASTValue v, ASTNS::AST const &ast) const {
    if (&v.type() == this)
        return IR::ASTValue(*v.val, ast);

    v = impl_cast(cgc, fun, cur_block, v); // try implicit cast to self

    if (&v.type() == this)
        return IR::ASTValue(*v.val, ast);

    IntType const *sty (dynamic_cast<IntType const *>(&v.type()));
    if (sty) {
        return IR::ASTValue(cur_block->add<IR::Instrs::IntToFloat>(v, this), ast);
    } else if (dynamic_cast<FloatType const *>(&v.type())) {
        return IR::ASTValue(cur_block->add<IR::Instrs::FloatToFloat>(v, this), ast);
    } else {
        ERR_INVALID_CAST(ast, v, *this);
        return Maybe<IR::ASTValue>();
    }
}
IR::ASTValue IR::FloatType::impl_cast(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, IR::ASTValue v) const {
    if (dynamic_cast<GenericFloatType const *>(&v.type()))
        return IR::ASTValue(cur_block->add<IR::Instrs::FloatToFloat>(v, this), *v.ast);

    return v;
}
// Int {{{2
IR::IntType::IntType(CodeGen::Context &context, ASTNS::AST const &decl_ast, int size, bool is_signed): Type(context), size(size), is_signed(is_signed), _decl_ast(decl_ast) {ASSERT(size == 1 || size == 8 || size == 16 || size == 32 || size == 64)}
ASTNS::AST const &IR::IntType::decl_ast() const {
    return *_decl_ast;
}

llvm::Type& IR::IntType::to_llvmtype(llvm::LLVMContext &con) const {
    return *llvm::IntegerType::get(con, size);
}
std::string IR::IntType::name() const {
    return format("{}int{}", is_signed ? 's' : 'u', size);
}
Maybe<IR::ASTValue> IR::IntType::bin_op(BIN_OP_ARGS) const {
    ASSERT(&l.type() == this)
    return int_bin_op(cgc, fun, cur_block, op, l, r, ast);
}
Maybe<IR::ASTValue> IR::IntType::unary_op(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, Located<ASTNS::UnaryOperator> op, IR::ASTValue v, ASTNS::AST const &ast) const {
    ASSERT(&v.type() == this)
    return int_unary_op(cgc, fun, cur_block, op, v, ast);
}
Maybe<IR::ASTValue> IR::IntType::cast_from(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, IR::ASTValue v, ASTNS::AST const &ast) const {
    if (&v.type() == this)
        return IR::ASTValue(*v.val, ast);

    v = impl_cast(cgc, fun, cur_block, v); // try implicit cast to self

    if (&v.type() == this)
        return IR::ASTValue(*v.val, ast);

    IntType const *sty_int (dynamic_cast<IntType const *> (&v.type()));
    FloatType const *sty_float (dynamic_cast<FloatType const *> (&v.type()));
    CharType const *sty_char (dynamic_cast<CharType const *> (&v.type()));
    BoolType const *sty_bool (dynamic_cast<BoolType const *> (&v.type()));

    if (sty_int || sty_char || sty_bool) {
        if (sty_char) {
            IR::IntType &newt (cgc.get_int_type(8, false));
            sty_int = &newt;
            sty_char = nullptr;

            v = IR::ASTValue(cur_block->add<IR::Instrs::NoOpCast>(v, newt), *v.ast);
        } else if (sty_bool) {
            IR::IntType &newt (cgc.get_int_type(1, false));
            sty_int = &newt;
            sty_bool = nullptr;

            v = IR::ASTValue(cur_block->add<IR::Instrs::NoOpCast>(v, newt), *v.ast);
        }

        return IR::ASTValue(cur_block->add<IR::Instrs::IntToInt>(v, this), ast);
    } else if (sty_float) {
        return IR::ASTValue(cur_block->add<IR::Instrs::FloatToInt>(v, this), ast);
    } else {
        ERR_INVALID_CAST(ast, v, *this);
        return Maybe<IR::ASTValue>();
    }
}
IR::ASTValue IR::IntType::impl_cast(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, IR::ASTValue v) const {
    if (dynamic_cast<GenericIntType const *>(&v.type()))
        return ASTValue(cur_block->add<IR::Instrs::IntToInt>(v, this), *v.ast);

    return v;
}
// Generic types {{{2
// GenericInt {{{2
IR::GenericIntType::GenericIntType(CodeGen::Context &context, ASTNS::AST const &decl_ast): Type(context), _decl_ast(decl_ast) {}
ASTNS::AST const &IR::GenericIntType::decl_ast() const {
    return *_decl_ast;
}

std::string IR::GenericIntType::name() const {
    return "<integer>";
}
Maybe<IR::ASTValue> IR::GenericIntType::bin_op(BIN_OP_ARGS) const {
    ASSERT(&l.type() == this);
    return int_bin_op(cgc, fun, cur_block, op, l, r, ast);
}
Maybe<IR::ASTValue> IR::GenericIntType::unary_op(UNARY_OP_ARGS) const {
    ASSERT(&v.type() == this);
    return int_unary_op(cgc, fun, cur_block, op, v, ast);
}
Maybe<IR::ASTValue> IR::GenericIntType::cast_from(CodeGen::Context &, IR::Function &, NNPtr<IR::Block> &, IR::ASTValue v, ASTNS::AST const &ast) const {
    ERR_INVALID_CAST(ast, v, *this);
    return Maybe<ASTValue>();
}
llvm::Type& IR::GenericIntType::to_llvmtype(llvm::LLVMContext &con) const {
    return *llvm::Type::getInt32Ty(con);
}
IR::ASTValue IR::GenericIntType::impl_cast(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, IR::ASTValue v) const {
    return v;
}
// GenericFloat {{{2
IR::GenericFloatType::GenericFloatType(CodeGen::Context &context, ASTNS::AST const &decl_ast): Type(context), _decl_ast(decl_ast) {}
ASTNS::AST const &IR::GenericFloatType::decl_ast() const {
    return *_decl_ast;
}

std::string IR::GenericFloatType::name() const {
    return "<float>";
}
Maybe<IR::ASTValue> IR::GenericFloatType::bin_op(BIN_OP_ARGS) const {
    ASSERT(&l.type() == this);
    return float_bin_op(cgc, fun, cur_block, op, l, r, ast);
}
Maybe<IR::ASTValue> IR::GenericFloatType::unary_op(UNARY_OP_ARGS) const {
    ASSERT(&v.type() == this);
    return float_unary_op(cgc, fun, cur_block, op, v, ast);
}
Maybe<IR::ASTValue> IR::GenericFloatType::cast_from(CodeGen::Context &, IR::Function &, NNPtr<IR::Block> &, IR::ASTValue v, ASTNS::AST const &ast) const {
    ERR_INVALID_CAST(ast, v, *this);
    return Maybe<ASTValue>();
}
llvm::Type& IR::GenericFloatType::to_llvmtype(llvm::LLVMContext &con) const {
    return *llvm::Type::getFloatTy(con);
}

IR::ASTValue IR::GenericFloatType::impl_cast(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, IR::ASTValue v) const {
    return v;
}
// Char {{{1
IR::CharType::CharType(CodeGen::Context &context, ASTNS::AST const &decl_ast): Type(context), _decl_ast(decl_ast) {}
ASTNS::AST const &IR::CharType::decl_ast() const {
    return *_decl_ast;
}

llvm::Type& IR::CharType::to_llvmtype(llvm::LLVMContext &con) const {
    return *llvm::Type::getInt8Ty(con);
}
std::string IR::CharType::name() const {
    return "char";
}
Maybe<IR::ASTValue> IR::CharType::bin_op(BIN_OP_ARGS) const {
    ASSERT(&l.type() == this)

    l = r.type().impl_cast(cgc, fun, cur_block, l);
    r = l.type().impl_cast(cgc, fun, cur_block, r);
    if (&l.type() != &r.type()) {
        ERR_CONFLICT_TYS_BINARY_OP(l, r, op);
        return Maybe<IR::ASTValue>();
    }

    switch (op.value) {
        SUPPORT_OPERATOR_BASIC(GREATER, ICmpGT)
        SUPPORT_OPERATOR_BASIC(LESS, ICmpLT)
        SUPPORT_OPERATOR_BASIC(GREATEREQUAL, ICmpGE)
        SUPPORT_OPERATOR_BASIC(LESSEQUAL, ICmpLE)
        SUPPORT_OPERATOR_BASIC(DOUBLEEQUAL, ICmpEQ)
        SUPPORT_OPERATOR_BASIC(BANGEQUAL, ICmpNE)

        default:
            ERR_LHS_UNSUPPORTED_OP(l, op);
            return Maybe<ASTValue>();
    }
}
Maybe<IR::ASTValue> IR::CharType::unary_op(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, Located<ASTNS::UnaryOperator> op, IR::ASTValue v, ASTNS::AST const &ast) const {
    ASSERT(&v.type() == this)

    ERR_UNARY_UNSUPPORTED_OP(v, op);
    return Maybe<ASTValue>();
}
Maybe<IR::ASTValue> IR::CharType::cast_from(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, IR::ASTValue v, ASTNS::AST const &ast) const {
    if (&v.type() == this)
        return IR::ASTValue(*v.val, ast);

    IntType const *sty (dynamic_cast<IntType const *>(&v.type()));
    if (!sty) {
        ERR_INVALID_CAST(ast, v, *this);
        return Maybe<IR::ASTValue>();
    }

    IR::IntType const &char_as_int_type (cgc.get_int_type(8, false));
    IR::Instrs::Instruction &as_int = cur_block->add<IR::Instrs::IntToInt>(v, char_as_int_type);

    return IR::ASTValue(cur_block->add<IR::Instrs::NoOpCast>(IR::ASTValue(as_int, *v.ast), this), ast);
}
IR::ASTValue IR::CharType::impl_cast(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, IR::ASTValue v) const {
    return v;
}
// Bool {{{1
IR::BoolType::BoolType(CodeGen::Context &context, ASTNS::AST const &decl_ast): Type(context), _decl_ast(decl_ast) {}
ASTNS::AST const &IR::BoolType::decl_ast() const {
    return *_decl_ast;
}

llvm::Type& IR::BoolType::to_llvmtype(llvm::LLVMContext &con) const {
    return *llvm::Type::getInt1Ty(con);
}
std::string IR::BoolType::name() const {
    return "bool";
}
Maybe<IR::ASTValue> IR::BoolType::bin_op(CodeGen::Context &cgc, Function &fun, NNPtr<Block> &cur_block, Located<ASTNS::BinaryOperator> op, IR::ASTValue l, IR::ASTValue r, ASTNS::AST const &ast) const {
    ASSERT(&l.type() == this)

    l = r.type().impl_cast(cgc, fun, cur_block, l);
    r = l.type().impl_cast(cgc, fun, cur_block, r);
    if (&l.type() != &r.type()) {
        ERR_CONFLICT_TYS_BINARY_OP(l, r, op);
        return Maybe<IR::ASTValue>();
    }

    switch (op.value) {
        SUPPORT_OPERATOR_BASIC(AMPER, BitAnd)
        SUPPORT_OPERATOR_BASIC(PIPE, BitOr)
        SUPPORT_OPERATOR_BASIC(CARET, BitXor)
        SUPPORT_OPERATOR_BASIC(DOUBLEEQUAL, ICmpEQ)
        SUPPORT_OPERATOR_BASIC(BANGEQUAL, ICmpNE)

        default:
            ERR_LHS_UNSUPPORTED_OP(l, op);
            return Maybe<ASTValue>();
    }
}
Maybe<IR::ASTValue> IR::BoolType::unary_op(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, Located<ASTNS::UnaryOperator> op, IR::ASTValue v, ASTNS::AST const &ast) const {
    ASSERT(&v.type() == this)

    switch (op.value) {
        case ASTNS::UnaryOperator::BANG:
            return IR::ASTValue(cur_block->add<IR::Instrs::Not>(v), ast);

        default:
            ERR_UNARY_UNSUPPORTED_OP(v, op);
            return Maybe<ASTValue>();
    }
}
Maybe<IR::ASTValue> IR::BoolType::cast_from(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, IR::ASTValue v, ASTNS::AST const &ast) const {
    if (&v.type() == this)
        return IR::ASTValue(*v.val, ast);

    IntType const *sty (dynamic_cast<IntType const *>(&v.type()));
    if (!sty) {
        ERR_INVALID_CAST(ast, v, *this);
        return Maybe<IR::ASTValue>();
    }

    IR::IntType const &bool_as_int_type (cgc.get_int_type(1, false));
    IR::Instrs::Instruction &as_int = cur_block->add<IR::Instrs::IntToInt>(v, bool_as_int_type);

    return IR::ASTValue(cur_block->add<IR::Instrs::NoOpCast>(ASTValue(as_int, *v.ast), this), ast);
}
IR::ASTValue IR::BoolType::impl_cast(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, IR::ASTValue v) const {
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
