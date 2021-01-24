#include "ir/type.h"
#include "ir/block.h"
#include "ir/instruction.h"
#include "message/errmsgs.h"
#include "utils/format.h"
#include "utils/assert.h"

#include "codegen/codegen.h"
#include "codegen/context.h"

#include "llvm/IR/DerivedTypes.h"

#define BIN_OP_ARGS CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, IR::Type::BinaryOperator op, IR::ASTValue l, IR::ASTValue r, Token optok, NNPtr<ASTNS::AST> ast
#define UNARY_OP_ARGS CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, IR::Type::UnaryOperator op, IR::ASTValue v, Token optok, NNPtr<ASTNS::AST> ast
// static functions {{{1
#define SUPPORT_OPERATOR_BASIC(op, instr) case IR::Type::BinaryOperator::op: return IR::ASTValue(cur_block->add<IR::Instrs::instr>(l, r), ast);
// float/int operations for reuse between generic float/int and concrete float/int types {{{2
static Maybe<IR::ASTValue> float_bin_op(BIN_OP_ARGS) {
    l = r.type()->impl_cast(cgc, fun, cur_block, l);
    r = l.type()->impl_cast(cgc, fun, cur_block, r);

    if (l.type() != r.type()) {
        ERR_CONFLICT_TYS_BINARY_OP(l, r, optok);
        return Maybe<IR::ASTValue>();
    }

    switch (op) {
        SUPPORT_OPERATOR_BASIC(plus, FAdd)
        SUPPORT_OPERATOR_BASIC(minus, FSub)
        SUPPORT_OPERATOR_BASIC(star, FMult)
        SUPPORT_OPERATOR_BASIC(slash, FDiv)
        SUPPORT_OPERATOR_BASIC(percent, FMod)
        SUPPORT_OPERATOR_BASIC(greater, FCmpGT)
        SUPPORT_OPERATOR_BASIC(less, FCmpLT)
        SUPPORT_OPERATOR_BASIC(greaterequal, FCmpGE)
        SUPPORT_OPERATOR_BASIC(lessequal, FCmpLE)
        SUPPORT_OPERATOR_BASIC(doubleequal, FCmpEQ)
        SUPPORT_OPERATOR_BASIC(bangequal, FCmpNE)

        default:
            ERR_LHS_UNSUPPORTED_OP(l, optok);
            return Maybe<IR::ASTValue>();
    }
}
static Maybe<IR::ASTValue> float_unary_op(UNARY_OP_ARGS) {
    switch (op) {
        case IR::Type::UnaryOperator::minus:
            return IR::ASTValue(cur_block->add<IR::Instrs::FNeg>(v), ast);

        default:
            ERR_UNARY_UNSUPPORTED_OP(v, optok);
            return Maybe<IR::ASTValue>();
    }
}
static Maybe<IR::ASTValue> int_bin_op(BIN_OP_ARGS) {
    l = r.type()->impl_cast(cgc, fun, cur_block, l);
    r = l.type()->impl_cast(cgc, fun, cur_block, r);

    if (l.type() != r.type()) {
        ERR_CONFLICT_TYS_BINARY_OP(l, r, optok);
        return Maybe<IR::ASTValue>();
    }

    switch (op) {
        SUPPORT_OPERATOR_BASIC(plus, IAdd)
        SUPPORT_OPERATOR_BASIC(minus, ISub)
        SUPPORT_OPERATOR_BASIC(star, IMult)
        SUPPORT_OPERATOR_BASIC(slash, IDiv)
        SUPPORT_OPERATOR_BASIC(percent, IMod)
        SUPPORT_OPERATOR_BASIC(greater, ICmpGT)
        SUPPORT_OPERATOR_BASIC(less, ICmpLT)
        SUPPORT_OPERATOR_BASIC(greaterequal, ICmpGE)
        SUPPORT_OPERATOR_BASIC(lessequal, ICmpLE)
        SUPPORT_OPERATOR_BASIC(doubleequal, ICmpEQ)
        SUPPORT_OPERATOR_BASIC(bangequal, ICmpNE)
        SUPPORT_OPERATOR_BASIC(amper, BitAnd)
        SUPPORT_OPERATOR_BASIC(pipe, BitOr)
        SUPPORT_OPERATOR_BASIC(caret, BitXor)
        SUPPORT_OPERATOR_BASIC(doublegreater, ShiftR)
        SUPPORT_OPERATOR_BASIC(doubleless, ShiftL)

        default:
            ERR_LHS_UNSUPPORTED_OP(l, optok);
            return Maybe<IR::ASTValue>();
    }
}
static Maybe<IR::ASTValue> int_unary_op(UNARY_OP_ARGS) {
    switch (op) {
        case IR::Type::UnaryOperator::tilde:
            return IR::ASTValue(cur_block->add<IR::Instrs::BitNot>(v), ast);

        case IR::Type::UnaryOperator::minus:
            return IR::ASTValue(cur_block->add<IR::Instrs::INeg>(v), ast);

        default:
            ERR_UNARY_UNSUPPORTED_OP(v, optok);
            return Maybe<IR::ASTValue>();
    }
}
// Float and Int {{{1
// Float {{{2
IR::FloatType::FloatType(CodeGen::Context &context, NNPtr<ASTNS::AST> decl_ast, int size): Type(context), size(size), _decl_ast(decl_ast) {ASSERT(size == 32 || size == 64)}
NNPtr<ASTNS::AST> IR::FloatType::decl_ast() const {
    return _decl_ast;
}

NNPtr<llvm::Type> IR::FloatType::to_llvmtype(llvm::LLVMContext &con) const {
    if (size == 32)
        return llvm::Type::getFloatTy(con);
    else if (size == 64)
        return llvm::Type::getDoubleTy(con);
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
Maybe<IR::ASTValue> IR::FloatType::bin_op(BIN_OP_ARGS) {
    ASSERT(l.type().as_raw() == this);
    return float_bin_op(cgc, fun, cur_block, op, l, r, optok, ast);
}
Maybe<IR::ASTValue> IR::FloatType::unary_op(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, IR::Type::UnaryOperator op, IR::ASTValue v, Token optok, NNPtr<ASTNS::AST> ast) {
    ASSERT(v.type().as_raw() == this);
    return float_unary_op(cgc, fun, cur_block, op, v, optok, ast);
}
Maybe<IR::ASTValue> IR::FloatType::cast_from(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, IR::ASTValue v, NNPtr<ASTNS::AST> ast) {
    if (v.type().as_raw() == this)
        return IR::ASTValue(v.val, ast);

    v = impl_cast(cgc, fun, cur_block, v); // try implicit cast to self

    if (v.type().as_raw() == this)
        return IR::ASTValue(v.val, ast);

    IntType *sty (dynamic_cast<IntType*>(v.type().as_raw()));
    if (sty) {
        return IR::ASTValue(cur_block->add<IR::Instrs::IntToFloat>(v, this), ast);
    } else if (dynamic_cast<FloatType*>(v.type().as_raw())) {
        return IR::ASTValue(cur_block->add<IR::Instrs::FloatToFloat>(v, this), ast);
    } else {
        ERR_INVALID_CAST(ast, v, this);
        return Maybe<IR::ASTValue>();
    }
}
IR::ASTValue IR::FloatType::impl_cast(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, IR::ASTValue v) {
    if (dynamic_cast<GenericFloatType*>(v.type().as_raw()))
        return IR::ASTValue(cur_block->add<IR::Instrs::FloatToFloat>(v, this), v.ast);

    return v;
}
// Int {{{2
IR::IntType::IntType(CodeGen::Context &context, NNPtr<ASTNS::AST> decl_ast, int size, bool is_signed): Type(context), size(size), is_signed(is_signed), _decl_ast(decl_ast) {ASSERT(size == 1 || size == 8 || size == 16 || size == 32 || size == 64)}
NNPtr<ASTNS::AST> IR::IntType::decl_ast() const {
    return _decl_ast;
}

NNPtr<llvm::Type> IR::IntType::to_llvmtype(llvm::LLVMContext &con) const {
    return llvm::IntegerType::get(con, size);
}
std::string IR::IntType::name() const {
    return format("{}int{}", is_signed ? 's' : 'u', size);
}
Maybe<IR::ASTValue> IR::IntType::bin_op(BIN_OP_ARGS) {
    ASSERT(l.type().as_raw() == this)
    return int_bin_op(cgc, fun, cur_block, op, l, r, optok, ast);
}
Maybe<IR::ASTValue> IR::IntType::unary_op(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, IR::Type::UnaryOperator op, IR::ASTValue v, Token optok, NNPtr<ASTNS::AST> ast) {
    ASSERT(v.type().as_raw() == this)
    return int_unary_op(cgc, fun, cur_block, op, v, optok, ast);
}
Maybe<IR::ASTValue> IR::IntType::cast_from(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, IR::ASTValue v, NNPtr<ASTNS::AST> ast) {
    if (v.type().as_raw() == this)
        return IR::ASTValue(v.val, ast);

    v = impl_cast(cgc, fun, cur_block, v); // try implicit cast to self

    if (v.type().as_raw() == this)
        return IR::ASTValue(v.val, ast);

    IntType *sty_int (dynamic_cast<IntType*> (v.type().as_raw()));
    FloatType *sty_float (dynamic_cast<FloatType*> (v.type().as_raw()));
    CharType *sty_char (dynamic_cast<CharType*> (v.type().as_raw()));
    BoolType *sty_bool (dynamic_cast<BoolType*> (v.type().as_raw()));

    if (sty_int || sty_char || sty_bool) {
        if (sty_char) {
            NNPtr<IR::IntType> newt (cgc.get_int_type(8, false));
            sty_int = newt.as_raw();
            sty_char = nullptr;

            v = IR::ASTValue(cur_block->add<IR::Instrs::NoOpCast>(v, newt), v.ast);
        } else if (sty_bool) {
            NNPtr<IR::IntType> newt (cgc.get_int_type(1, false));
            sty_int = newt.as_raw();
            sty_bool = nullptr;

            v = IR::ASTValue(cur_block->add<IR::Instrs::NoOpCast>(v, newt), v.ast);
        }

        return IR::ASTValue(cur_block->add<IR::Instrs::IntToInt>(v, this), ast);
    } else if (sty_float) {
        return IR::ASTValue(cur_block->add<IR::Instrs::FloatToInt>(v, this), ast);
    } else {
        ERR_INVALID_CAST(ast, v, this);
        return Maybe<IR::ASTValue>();
    }
}
IR::ASTValue IR::IntType::impl_cast(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, IR::ASTValue v) {
    if (dynamic_cast<GenericIntType*>(v.type().as_raw()))
        return ASTValue(cur_block->add<IR::Instrs::IntToInt>(v, this), v.ast);

    return v;
}
// Generic types {{{2
// GenericInt {{{2
IR::GenericIntType::GenericIntType(CodeGen::Context &context, NNPtr<ASTNS::AST> decl_ast): Type(context), _decl_ast(decl_ast) {}
NNPtr<ASTNS::AST> IR::GenericIntType::decl_ast() const {
    return _decl_ast;
}

std::string IR::GenericIntType::name() const {
    return "<integer>";
}
Maybe<IR::ASTValue> IR::GenericIntType::bin_op(BIN_OP_ARGS) {
    ASSERT(l.type().as_raw() == this);
    return int_bin_op(cgc, fun, cur_block, op, l, r, optok, ast);
}
Maybe<IR::ASTValue> IR::GenericIntType::unary_op(UNARY_OP_ARGS) {
    ASSERT(v.type().as_raw() == this);
    return int_unary_op(cgc, fun, cur_block, op, v, optok, ast);
}
Maybe<IR::ASTValue> IR::GenericIntType::cast_from(CodeGen::Context &, IR::Function &, NNPtr<IR::Block> &, IR::ASTValue v, NNPtr<ASTNS::AST> ast) {
    ERR_INVALID_CAST(ast, v, this);
    return Maybe<ASTValue>();
}
NNPtr<llvm::Type> IR::GenericIntType::to_llvmtype(llvm::LLVMContext &con) const {
    return llvm::Type::getInt32Ty(con);
}
IR::ASTValue IR::GenericIntType::impl_cast(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, IR::ASTValue v) {
    return v;
}
// GenericFloat {{{2
IR::GenericFloatType::GenericFloatType(CodeGen::Context &context, NNPtr<ASTNS::AST> decl_ast): Type(context), _decl_ast(decl_ast) {}
NNPtr<ASTNS::AST> IR::GenericFloatType::decl_ast() const {
    return _decl_ast;
}

std::string IR::GenericFloatType::name() const {
    return "<float>";
}
Maybe<IR::ASTValue> IR::GenericFloatType::bin_op(BIN_OP_ARGS) {
    ASSERT(l.type().as_raw() == this);
    return float_bin_op(cgc, fun, cur_block, op, l, r, optok, ast);
}
Maybe<IR::ASTValue> IR::GenericFloatType::unary_op(UNARY_OP_ARGS) {
    ASSERT(v.type().as_raw() == this);
    return float_unary_op(cgc, fun, cur_block, op, v, optok, ast);
}
Maybe<IR::ASTValue> IR::GenericFloatType::cast_from(CodeGen::Context &, IR::Function &, NNPtr<IR::Block> &, IR::ASTValue v, NNPtr<ASTNS::AST> ast) {
    ERR_INVALID_CAST(ast, v, this);
    return Maybe<ASTValue>();
}
NNPtr<llvm::Type> IR::GenericFloatType::to_llvmtype(llvm::LLVMContext &con) const {
    return llvm::Type::getFloatTy(con);
}

IR::ASTValue IR::GenericFloatType::impl_cast(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, IR::ASTValue v) {
    return v;
}
// Char {{{1
IR::CharType::CharType(CodeGen::Context &context, NNPtr<ASTNS::AST> decl_ast): Type(context), _decl_ast(decl_ast) {}
NNPtr<ASTNS::AST> IR::CharType::decl_ast() const {
    return _decl_ast;
}

NNPtr<llvm::Type> IR::CharType::to_llvmtype(llvm::LLVMContext &con) const {
    return llvm::Type::getInt8Ty(con);
}
std::string IR::CharType::name() const {
    return "char";
}
Maybe<IR::ASTValue> IR::CharType::bin_op(BIN_OP_ARGS) {
    ASSERT(l.type().as_raw() == this)

    l = r.type()->impl_cast(cgc, fun, cur_block, l);
    r = l.type()->impl_cast(cgc, fun, cur_block, r);
    if (l.type() != r.type()) {
        ERR_CONFLICT_TYS_BINARY_OP(l, r, optok);
        return Maybe<IR::ASTValue>();
    }

    switch (op) {
        SUPPORT_OPERATOR_BASIC(greater, ICmpGT)
        SUPPORT_OPERATOR_BASIC(less, ICmpLT)
        SUPPORT_OPERATOR_BASIC(greaterequal, ICmpGE)
        SUPPORT_OPERATOR_BASIC(lessequal, ICmpLE)
        SUPPORT_OPERATOR_BASIC(doubleequal, ICmpEQ)
        SUPPORT_OPERATOR_BASIC(bangequal, ICmpNE)

        default:
            ERR_LHS_UNSUPPORTED_OP(l, optok);
            return Maybe<ASTValue>();
    }
}
Maybe<IR::ASTValue> IR::CharType::unary_op(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, IR::Type::UnaryOperator op, IR::ASTValue v, Token optok, NNPtr<ASTNS::AST> ast) {
    ASSERT(v.type().as_raw() == this)

    ERR_UNARY_UNSUPPORTED_OP(v, optok);
    return Maybe<ASTValue>();
}
Maybe<IR::ASTValue> IR::CharType::cast_from(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, IR::ASTValue v, NNPtr<ASTNS::AST> ast) {
    if (v.type().as_raw() == this)
        return IR::ASTValue(v.val, ast);

    IntType *sty (dynamic_cast<IntType*>(v.type().as_raw()));
    if (!sty) {
        ERR_INVALID_CAST(ast, v, this);
        return Maybe<IR::ASTValue>();
    }

    NNPtr<IR::IntType> char_as_int_type (cgc.get_int_type(8, false));
    NNPtr<IR::Instrs::Instruction> as_int = cur_block->add<IR::Instrs::IntToInt>(v, char_as_int_type);

    return IR::ASTValue(cur_block->add<IR::Instrs::NoOpCast>(IR::ASTValue(as_int, v.ast), this), ast);
}
IR::ASTValue IR::CharType::impl_cast(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, IR::ASTValue v) {
    return v;
}
// Bool {{{1
IR::BoolType::BoolType(CodeGen::Context &context, NNPtr<ASTNS::AST> decl_ast): Type(context), _decl_ast(decl_ast) {}
NNPtr<ASTNS::AST> IR::BoolType::decl_ast() const {
    return _decl_ast;
}

NNPtr<llvm::Type> IR::BoolType::to_llvmtype(llvm::LLVMContext &con) const {
    return llvm::Type::getInt1Ty(con);
}
std::string IR::BoolType::name() const {
    return "bool";
}
Maybe<IR::ASTValue> IR::BoolType::bin_op(CodeGen::Context &cgc, Function &fun, NNPtr<Block> &cur_block, BinaryOperator op, IR::ASTValue l, IR::ASTValue r, Token optok, NNPtr<ASTNS::AST> ast) {
    ASSERT(l.type().as_raw() == this)

    l = r.type()->impl_cast(cgc, fun, cur_block, l);
    r = l.type()->impl_cast(cgc, fun, cur_block, r);
    if (l.type() != r.type()) {
        ERR_CONFLICT_TYS_BINARY_OP(l, r, optok);
        return Maybe<IR::ASTValue>();
    }

    switch (op) {
        SUPPORT_OPERATOR_BASIC(amper, BitAnd)
        SUPPORT_OPERATOR_BASIC(pipe, BitOr)
        SUPPORT_OPERATOR_BASIC(caret, BitXor)
        SUPPORT_OPERATOR_BASIC(doubleequal, ICmpEQ)
        SUPPORT_OPERATOR_BASIC(bangequal, ICmpNE)

        default:
            ERR_LHS_UNSUPPORTED_OP(l, optok);
            return Maybe<ASTValue>();
    }
}
Maybe<IR::ASTValue> IR::BoolType::unary_op(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, IR::Type::UnaryOperator op, IR::ASTValue v, Token optok, NNPtr<ASTNS::AST> ast) {
    ASSERT(v.type().as_raw() == this)

    switch (op) {
        case Type::UnaryOperator::bang:
            return IR::ASTValue(cur_block->add<IR::Instrs::Not>(v), ast);

        default:
            ERR_UNARY_UNSUPPORTED_OP(v, optok);
            return Maybe<ASTValue>();
    }
}
Maybe<IR::ASTValue> IR::BoolType::cast_from(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, IR::ASTValue v, NNPtr<ASTNS::AST> ast) {
    if (v.type().as_raw() == this)
        return IR::ASTValue(v.val, ast);

    IntType *sty (dynamic_cast<IntType*>(v.type().as_raw()));
    if (!sty) {
        ERR_INVALID_CAST(ast, v, this);
        return Maybe<IR::ASTValue>();
    }

    NNPtr<IR::IntType> bool_as_int_type (cgc.get_int_type(1, false));
    NNPtr<IR::Instrs::Instruction> as_int = cur_block->add<IR::Instrs::IntToInt>(v, bool_as_int_type);

    return IR::ASTValue(cur_block->add<IR::Instrs::NoOpCast>(ASTValue(as_int, v.ast), this), ast);
}
IR::ASTValue IR::BoolType::impl_cast(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, IR::ASTValue v) {
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
