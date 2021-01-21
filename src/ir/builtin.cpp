#include "ir/type.h"
#include "ir/instruction.h"
#include "message/errmsgs.h"
#include "utils/format.h"
#include "utils/assert.h"

#include "codegen/codegen.h"
#include "codegen/context.h"

#include "llvm/IR/DerivedTypes.h"

#define BIN_OP_ARGS CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &curBlock, IR::Type::BinaryOperator op, IR::ASTValue l, IR::ASTValue r, Token optok, NNPtr<ASTNS::AST> ast
#define UNARY_OP_ARGS CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &curBlock, IR::Type::UnaryOperator op, IR::ASTValue v, Token optok, NNPtr<ASTNS::AST> ast
// static functions {{{1
#define SUPPORT_OPERATOR_BASIC(op, instr) case IR::Type::BinaryOperator::op: return IR::ASTValue(curBlock->add(std::make_unique<IR::Instrs::instr>(l, r)), ast);
// float/int operations for reuse between generic float/int and concrete float/int types {{{2
static Maybe<IR::ASTValue> floatBinOp(BIN_OP_ARGS) {
    l = r.type()->implCast(cgc, fun, curBlock, l);
    r = l.type()->implCast(cgc, fun, curBlock, r);

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
static Maybe<IR::ASTValue> floatUnaryOp(UNARY_OP_ARGS) {
    switch (op) {
        case IR::Type::UnaryOperator::minus:
            return IR::ASTValue(curBlock->add(std::make_unique<IR::Instrs::FNeg>(v)), ast);

        default:
            ERR_UNARY_UNSUPPORTED_OP(v, optok);
            return Maybe<IR::ASTValue>();
    }
}
static Maybe<IR::ASTValue> intBinOp(BIN_OP_ARGS) {
    l = r.type()->implCast(cgc, fun, curBlock, l);
    r = l.type()->implCast(cgc, fun, curBlock, r);

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
static Maybe<IR::ASTValue> intUnaryOp(UNARY_OP_ARGS) {
    switch (op) {
        case IR::Type::UnaryOperator::tilde:
            return IR::ASTValue(curBlock->add(std::make_unique<IR::Instrs::BitNot>(v)), ast);

        case IR::Type::UnaryOperator::minus:
            return IR::ASTValue(curBlock->add(std::make_unique<IR::Instrs::INeg>(v)), ast);

        default:
            ERR_UNARY_UNSUPPORTED_OP(v, optok);
            return Maybe<IR::ASTValue>();
    }
}
// Float and Int {{{1
// Float {{{2
IR::FloatType::FloatType(CodeGen::Context &context, NNPtr<ASTNS::AST> declAST, int size): Type(context), size(size), _declAST(declAST) {ASSERT(size == 32 || size == 64)}
NNPtr<ASTNS::AST> IR::FloatType::declAST() const {
    return _declAST;
}

NNPtr<llvm::Type> IR::FloatType::toLLVMType(llvm::LLVMContext &con) const {
    if (size == 32)
        return llvm::Type::getFloatTy(con);
    else if (size == 64)
        return llvm::Type::getDoubleTy(con);
    else
        reportAbortNoh(format("FloatType::toLLVMType: size = %", size));
}
std::string IR::FloatType::name() const {
    if (size == 32)
        return "float";
    else if (size == 64)
        return "double";
    else
        reportAbortNoh(format("FloatType::name: size = %", size));
}
Maybe<IR::ASTValue> IR::FloatType::binOp(BIN_OP_ARGS) {
    ASSERT(l.type().asRaw() == this);
    return floatBinOp(cgc, fun, curBlock, op, l, r, optok, ast);
}
Maybe<IR::ASTValue> IR::FloatType::unaryOp(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &curBlock, IR::Type::UnaryOperator op, IR::ASTValue v, Token optok, NNPtr<ASTNS::AST> ast) {
    ASSERT(v.type().asRaw() == this);
    return floatUnaryOp(cgc, fun, curBlock, op, v, optok, ast);
}
Maybe<IR::ASTValue> IR::FloatType::castFrom(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &curBlock, IR::ASTValue v, NNPtr<ASTNS::AST> ast) {
    if (v.type().asRaw() == this)
        return IR::ASTValue(v.val, ast);

    v = implCast(cgc, fun, curBlock, v); // try implicit cast to self

    if (v.type().asRaw() == this)
        return IR::ASTValue(v.val, ast);

    IntType *sty (dynamic_cast<IntType*>(v.type().asRaw()));
    if (sty) {
        return IR::ASTValue(curBlock->add(std::make_unique<IR::Instrs::IntToFloat>(v, this)), ast);
    } else if (dynamic_cast<FloatType*>(v.type().asRaw())) {
        return IR::ASTValue(curBlock->add(std::make_unique<IR::Instrs::FloatToFloat>(v, this)), ast);
    } else {
        ERR_INVALID_CAST(ast, v, this);
        return Maybe<IR::ASTValue>();
    }
}
IR::ASTValue IR::FloatType::implCast(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &curBlock, IR::ASTValue v) {
    if (dynamic_cast<GenericFloatType*>(v.type().asRaw()))
        return IR::ASTValue(curBlock->add(std::make_unique<IR::Instrs::FloatToFloat>(v, this)), v.ast);

    return v;
}
// Int {{{2
IR::IntType::IntType(CodeGen::Context &context, NNPtr<ASTNS::AST> declAST, int size, bool isSigned): Type(context), size(size), isSigned(isSigned), _declAST(declAST) {ASSERT(size == 1 || size == 8 || size == 16 || size == 32 || size == 64)}
NNPtr<ASTNS::AST> IR::IntType::declAST() const {
    return _declAST;
}

NNPtr<llvm::Type> IR::IntType::toLLVMType(llvm::LLVMContext &con) const {
    return llvm::IntegerType::get(con, size);
}
std::string IR::IntType::name() const {
    return format("%int%", isSigned ? 's' : 'u', size);
}
Maybe<IR::ASTValue> IR::IntType::binOp(BIN_OP_ARGS) {
    ASSERT(l.type().asRaw() == this)
    return intBinOp(cgc, fun, curBlock, op, l, r, optok, ast);
}
Maybe<IR::ASTValue> IR::IntType::unaryOp(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &curBlock, IR::Type::UnaryOperator op, IR::ASTValue v, Token optok, NNPtr<ASTNS::AST> ast) {
    ASSERT(v.type().asRaw() == this)
    return intUnaryOp(cgc, fun, curBlock, op, v, optok, ast);
}
Maybe<IR::ASTValue> IR::IntType::castFrom(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &curBlock, IR::ASTValue v, NNPtr<ASTNS::AST> ast) {
    if (v.type().asRaw() == this)
        return IR::ASTValue(v.val, ast);

    v = implCast(cgc, fun, curBlock, v); // try implicit cast to self

    if (v.type().asRaw() == this)
        return IR::ASTValue(v.val, ast);

    IntType *styInt (dynamic_cast<IntType*> (v.type().asRaw()));
    FloatType *styFloat (dynamic_cast<FloatType*> (v.type().asRaw()));
    CharType *styChar (dynamic_cast<CharType*> (v.type().asRaw()));
    BoolType *styBool (dynamic_cast<BoolType*> (v.type().asRaw()));

    if (styInt || styChar || styBool) {
        if (styChar) {
            NNPtr<IR::IntType> newt (cgc.getIntType(8, false));
            styInt = newt.asRaw();
            styChar = nullptr;

            v = IR::ASTValue(curBlock->add(std::make_unique<IR::Instrs::NoOpCast>(v, newt)), v.ast);
        } else if (styBool) {
            NNPtr<IR::IntType> newt (cgc.getIntType(1, false));
            styInt = newt.asRaw();
            styBool = nullptr;

            v = IR::ASTValue(curBlock->add(std::make_unique<IR::Instrs::NoOpCast>(v, newt)), v.ast);
        }

        return IR::ASTValue(curBlock->add(std::make_unique<IR::Instrs::IntToInt>(v, this)), ast);
    } else if (styFloat) {
        return IR::ASTValue(curBlock->add(std::make_unique<IR::Instrs::FloatToInt>(v, this)), ast);
    } else {
        ERR_INVALID_CAST(ast, v, this);
        return Maybe<IR::ASTValue>();
    }
}
IR::ASTValue IR::IntType::implCast(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &curBlock, IR::ASTValue v) {
    if (dynamic_cast<GenericIntType*>(v.type().asRaw()))
        return ASTValue(curBlock->add(std::make_unique<IR::Instrs::IntToInt>(v, this)), v.ast);

    return v;
}
// Generic types {{{2
// GenericInt {{{2
IR::GenericIntType::GenericIntType(CodeGen::Context &context, NNPtr<ASTNS::AST> declAST): Type(context), _declAST(declAST) {}
NNPtr<ASTNS::AST> IR::GenericIntType::declAST() const {
    return _declAST;
}

std::string IR::GenericIntType::name() const {
    return "<integer>";
}
Maybe<IR::ASTValue> IR::GenericIntType::binOp(BIN_OP_ARGS) {
    ASSERT(l.type().asRaw() == this);
    return intBinOp(cgc, fun, curBlock, op, l, r, optok, ast);
}
Maybe<IR::ASTValue> IR::GenericIntType::unaryOp(UNARY_OP_ARGS) {
    ASSERT(v.type().asRaw() == this);
    return intUnaryOp(cgc, fun, curBlock, op, v, optok, ast);
}
Maybe<IR::ASTValue> IR::GenericIntType::castFrom(CodeGen::Context &, IR::Function &, NNPtr<IR::Block> &, IR::ASTValue v, NNPtr<ASTNS::AST> ast) {
    ERR_INVALID_CAST(ast, v, this);
    return Maybe<ASTValue>();
}
NNPtr<llvm::Type> IR::GenericIntType::toLLVMType(llvm::LLVMContext &con) const {
    return llvm::Type::getInt32Ty(con);
}
IR::ASTValue IR::GenericIntType::implCast(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &curBlock, IR::ASTValue v) {
    return v;
}
// GenericFloat {{{2
IR::GenericFloatType::GenericFloatType(CodeGen::Context &context, NNPtr<ASTNS::AST> declAST): Type(context), _declAST(declAST) {}
NNPtr<ASTNS::AST> IR::GenericFloatType::declAST() const {
    return _declAST;
}

std::string IR::GenericFloatType::name() const {
    return "<float>";
}
Maybe<IR::ASTValue> IR::GenericFloatType::binOp(BIN_OP_ARGS) {
    ASSERT(l.type().asRaw() == this);
    return floatBinOp(cgc, fun, curBlock, op, l, r, optok, ast);
}
Maybe<IR::ASTValue> IR::GenericFloatType::unaryOp(UNARY_OP_ARGS) {
    ASSERT(v.type().asRaw() == this);
    return floatUnaryOp(cgc, fun, curBlock, op, v, optok, ast);
}
Maybe<IR::ASTValue> IR::GenericFloatType::castFrom(CodeGen::Context &, IR::Function &, NNPtr<IR::Block> &, IR::ASTValue v, NNPtr<ASTNS::AST> ast) {
    ERR_INVALID_CAST(ast, v, this);
    return Maybe<ASTValue>();
}
NNPtr<llvm::Type> IR::GenericFloatType::toLLVMType(llvm::LLVMContext &con) const {
    return llvm::Type::getFloatTy(con);
}

IR::ASTValue IR::GenericFloatType::implCast(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &curBlock, IR::ASTValue v) {
    return v;
}
// Char {{{1
IR::CharType::CharType(CodeGen::Context &context, NNPtr<ASTNS::AST> declAST): Type(context), _declAST(declAST) {}
NNPtr<ASTNS::AST> IR::CharType::declAST() const {
    return _declAST;
}

NNPtr<llvm::Type> IR::CharType::toLLVMType(llvm::LLVMContext &con) const {
    return llvm::Type::getInt8Ty(con);
}
std::string IR::CharType::name() const {
    return "char";
}
Maybe<IR::ASTValue> IR::CharType::binOp(BIN_OP_ARGS) {
    ASSERT(l.type().asRaw() == this)

    l = r.type()->implCast(cgc, fun, curBlock, l);
    r = l.type()->implCast(cgc, fun, curBlock, r);
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
Maybe<IR::ASTValue> IR::CharType::unaryOp(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &curBlock, IR::Type::UnaryOperator op, IR::ASTValue v, Token optok, NNPtr<ASTNS::AST> ast) {
    ASSERT(v.type().asRaw() == this)

    ERR_UNARY_UNSUPPORTED_OP(v, optok);
    return Maybe<ASTValue>();
}
Maybe<IR::ASTValue> IR::CharType::castFrom(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &curBlock, IR::ASTValue v, NNPtr<ASTNS::AST> ast) {
    if (v.type().asRaw() == this)
        return IR::ASTValue(v.val, ast);

    IntType *sty (dynamic_cast<IntType*>(v.type().asRaw()));
    if (!sty) {
        ERR_INVALID_CAST(ast, v, this);
        return Maybe<IR::ASTValue>();
    }

    NNPtr<IR::IntType> charAsIntType (cgc.getIntType(8, false));
    NNPtr<IR::Instrs::Instruction> asInt = curBlock->add(std::make_unique<IR::Instrs::IntToInt>(v, charAsIntType));

    return IR::ASTValue(curBlock->add(std::make_unique<IR::Instrs::NoOpCast>(IR::ASTValue(asInt, v.ast), this)), ast);
}
IR::ASTValue IR::CharType::implCast(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &curBlock, IR::ASTValue v) {
    return v;
}
// Bool {{{1
IR::BoolType::BoolType(CodeGen::Context &context, NNPtr<ASTNS::AST> declAST): Type(context), _declAST(declAST) {}
NNPtr<ASTNS::AST> IR::BoolType::declAST() const {
    return _declAST;
}

NNPtr<llvm::Type> IR::BoolType::toLLVMType(llvm::LLVMContext &con) const {
    return llvm::Type::getInt1Ty(con);
}
std::string IR::BoolType::name() const {
    return "bool";
}
Maybe<IR::ASTValue> IR::BoolType::binOp(CodeGen::Context &cgc, Function &fun, NNPtr<Block> &curBlock, BinaryOperator op, IR::ASTValue l, IR::ASTValue r, Token optok, NNPtr<ASTNS::AST> ast) {
    ASSERT(l.type().asRaw() == this)

    l = r.type()->implCast(cgc, fun, curBlock, l);
    r = l.type()->implCast(cgc, fun, curBlock, r);
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
Maybe<IR::ASTValue> IR::BoolType::unaryOp(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &curBlock, IR::Type::UnaryOperator op, IR::ASTValue v, Token optok, NNPtr<ASTNS::AST> ast) {
    ASSERT(v.type().asRaw() == this)

    switch (op) {
        case Type::UnaryOperator::bang:
            return IR::ASTValue(curBlock->add(std::make_unique<IR::Instrs::Not>(v)), ast);

        default:
            ERR_UNARY_UNSUPPORTED_OP(v, optok);
            return Maybe<ASTValue>();
    }
}
Maybe<IR::ASTValue> IR::BoolType::castFrom(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &curBlock, IR::ASTValue v, NNPtr<ASTNS::AST> ast) {
    if (v.type().asRaw() == this)
        return IR::ASTValue(v.val, ast);

    IntType *sty (dynamic_cast<IntType*>(v.type().asRaw()));
    if (!sty) {
        ERR_INVALID_CAST(ast, v, this);
        return Maybe<IR::ASTValue>();
    }

    NNPtr<IR::IntType> boolAsIntType (cgc.getIntType(1, false));
    NNPtr<IR::Instrs::Instruction> asInt = curBlock->add(std::make_unique<IR::Instrs::IntToInt>(v, boolAsIntType));

    return IR::ASTValue(curBlock->add(std::make_unique<IR::Instrs::NoOpCast>(ASTValue(asInt, v.ast), this)), ast);
}
IR::ASTValue IR::BoolType::implCast(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &curBlock, IR::ASTValue v) {
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
