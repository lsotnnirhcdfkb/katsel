#include <sstream>

#include "ir/type.h"
#include "ir/instruction.h"
#include "message/internal.h"
#include "message/errmsgs.h"
#include "utils/format.h"
#include "utils/assert.h"

#include "codegen/codegen.h"
#include "codegen/context.h"

#include "llvm/IR/DerivedTypes.h"

#define BIN_OP_ARGS CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::Type::BinaryOperator op, IR::ASTValue l, IR::ASTValue r, Token optok, ASTNS::AST *ast
#define UNARY_OP_ARGS CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::Type::UnaryOperator op, IR::ASTValue v, Token optok, ASTNS::AST *ast
// static functions {{{1
static IR::Type* defaultBinOpRetTy(CodeGen::Context &cgc, IR::Type *ltype, IR::Type::BinaryOperator op)
{
    switch (op)
    {
        case IR::Type::BinaryOperator::doublepipe:
        case IR::Type::BinaryOperator::doubleamper:
        case IR::Type::BinaryOperator::bangequal:
        case IR::Type::BinaryOperator::doubleequal:
        case IR::Type::BinaryOperator::less:
        case IR::Type::BinaryOperator::greater:
        case IR::Type::BinaryOperator::lessequal:
        case IR::Type::BinaryOperator::greaterequal:
            return cgc.getBoolType();

        case IR::Type::BinaryOperator::caret:
        case IR::Type::BinaryOperator::pipe:
        case IR::Type::BinaryOperator::amper:
        case IR::Type::BinaryOperator::doubleless:
        case IR::Type::BinaryOperator::doublegreater:
        case IR::Type::BinaryOperator::plus:
        case IR::Type::BinaryOperator::minus:
        case IR::Type::BinaryOperator::star:
        case IR::Type::BinaryOperator::slash:
        case IR::Type::BinaryOperator::percent:
            return ltype;
    }
}
#define SUPPORT_OPERATOR_BASIC(op, instr) case IR::Type::BinaryOperator::op: curBlock->add(std::make_unique<IR::Instrs::instr>(retReg, l, r)); break;
// float/int operations for reuse between generic float/int and concrete float/int types {{{1
static IR::ASTValue floatBinOp(BIN_OP_ARGS)
{
    l = r.type()->implCast(cgc, fun, curBlock, l);
    r = l.type()->implCast(cgc, fun, curBlock, r);
    if (l.type() != r.type())
    {
        ERR_CONFLICT_TYS_BINARY_OP(l, r, optok);
        return IR::ASTValue();
    }

    IR::Type *retTy (defaultBinOpRetTy(cgc, l.type(), op));
    IR::TempRegister *retReg (fun.addTempRegister(retTy));

    switch (op)
    {
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
            ERR_LHS_UNSUPPORTED_OP(l, r, optok);
            return IR::ASTValue();
    }

    return IR::ASTValue(retReg, ast);
}
static IR::ASTValue floatUnaryOp(UNARY_OP_ARGS)
{
    IR::TempRegister *outReg = fun.addTempRegister(v.type());
    switch (op)
    {
        case IR::Type::UnaryOperator::minus:
            curBlock->add(std::make_unique<IR::Instrs::FNeg>(outReg, v));
            break;

        default:
            ERR_UNARY_UNSUPPORTED_OP(v, optok);
            return IR::ASTValue();
    }

    return IR::ASTValue(outReg, ast);
}
static IR::ASTValue intBinOp(BIN_OP_ARGS)
{
    l = r.type()->implCast(cgc, fun, curBlock, l);
    r = l.type()->implCast(cgc, fun, curBlock, r);
    if (l.type() != r.type())
    {
        ERR_CONFLICT_TYS_BINARY_OP(l, r, optok);
        return IR::ASTValue();
    }

    IR::Type *retTy (defaultBinOpRetTy(cgc, l.type(), op));
    IR::TempRegister *retReg (fun.addTempRegister(retTy));

    switch (op)
    {
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
            ERR_LHS_UNSUPPORTED_OP(l, r, optok);
            return IR::ASTValue();
    }

    return IR::ASTValue(retReg, ast);
}
static IR::ASTValue intUnaryOp(UNARY_OP_ARGS)
{
    IR::TempRegister *outReg = fun.addTempRegister(v.type());
    switch (op)
    {
        case IR::Type::UnaryOperator::tilde:
            curBlock->add(std::make_unique<IR::Instrs::BitNot>(outReg, v));
            break;

        case IR::Type::UnaryOperator::minus:
            curBlock->add(std::make_unique<IR::Instrs::INeg>(outReg, v));
            break;

        default:
            ERR_UNARY_UNSUPPORTED_OP(v, optok);
            return IR::ASTValue();
    }

    return IR::ASTValue(outReg, ast);
}
// Float and Int {{{1
// Float {{{2
IR::FloatType::FloatType(int size): size(size) {ASSERT(size == 32 || size == 64)}
llvm::Type* IR::FloatType::toLLVMType(llvm::LLVMContext &con) const
{
    if (size == 32)
        return llvm::Type::getFloatTy(con);
    else if (size == 64)
        return llvm::Type::getDoubleTy(con);
    else
        reportAbortNoh(format("FloatType::toLLVMType: size = %", size));
}
std::string IR::FloatType::stringify() const
{
    if (size == 32)
        return "float";
    else if (size == 64)
        return "double";
    else
        reportAbortNoh(format("FloatType::stringify: size = %", size));
}
IR::ASTValue IR::FloatType::binOp(BIN_OP_ARGS)
{
    ASSERT(l.type() == this);
    return floatBinOp(cgc, fun, curBlock, op, l, r, optok, ast);
}
IR::ASTValue IR::FloatType::unaryOp(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::Type::UnaryOperator op, IR::ASTValue v, Token optok, ASTNS::AST *ast)
{
    ASSERT(v.type() == this);
    return floatUnaryOp(cgc, fun, curBlock, op, v, optok, ast);
}
IR::ASTValue IR::FloatType::castTo(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::ASTValue v, ASTNS::AST *ast)
{
    if (v.type() == this)
        return IR::ASTValue(v.val, ast);

    v = implCast(cgc, fun, curBlock, v); // try implicit cast to self

    if (v.type() == this)
        return IR::ASTValue(v.val, ast);

    IntType *sty (dynamic_cast<IntType*>(v.type()));
    if (sty)
    {
        IR::TempRegister *outReg = fun.addTempRegister(this);
        curBlock->add(std::make_unique<IR::Instrs::IntToFloat>(outReg, v, this));
        return ASTValue(outReg, ast);
    }
    else if (FloatType *styf = dynamic_cast<FloatType*>(v.type()))
    {
        IR::TempRegister *outReg = fun.addTempRegister(this);
        curBlock->add(std::make_unique<IR::Instrs::FloatToFloat>(outReg, v, this));
        return ASTValue(outReg, ast);
    }
    else
    {
        ERR_INVALID_CAST(ast, v, this);
        return IR::ASTValue();
    }
}
IR::ASTValue IR::FloatType::implCast(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::ASTValue v)
{
    if (dynamic_cast<GenericFloatType*>(v.type()))
    {
        IR::TempRegister *concreteReg = fun.addTempRegister(this);
        curBlock->add(std::make_unique<IR::Instrs::FloatToFloat>(concreteReg, v, this));
        return ASTValue(concreteReg, v.ast);
    }
    return v;
}
// Int {{{2
IR::IntType::IntType(int size, bool isSigned): size(size), isSigned(isSigned) {ASSERT(size == 1 || size == 8 || size == 16 || size == 32 || size == 64)}
llvm::Type* IR::IntType::toLLVMType(llvm::LLVMContext &con) const
{
    return llvm::IntegerType::get(con, size);
}
std::string IR::IntType::stringify() const
{
    return format("%int%", isSigned ? 's' : 'u', size);
}
IR::ASTValue IR::IntType::binOp(BIN_OP_ARGS)
{
    ASSERT(l.type() == this)
    return intBinOp(cgc, fun, curBlock, op, l, r, optok, ast);
}
IR::ASTValue IR::IntType::unaryOp(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::Type::UnaryOperator op, IR::ASTValue v, Token optok, ASTNS::AST *ast)
{
    ASSERT(v.type() == this)
    return intUnaryOp(cgc, fun, curBlock, op, v, optok, ast);
}
IR::ASTValue IR::IntType::castTo(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::ASTValue v, ASTNS::AST *ast)
{
    if (v.type() == this)
        return IR::ASTValue(v.val, ast);

    v = implCast(cgc, fun, curBlock, v); // try implicit cast to self

    if (v.type() == this)
        return IR::ASTValue(v.val, ast);

    IntType *styInt (dynamic_cast<IntType*> (v.type()));
    FloatType *styFloat (dynamic_cast<FloatType*> (v.type()));
    CharType *styChar (dynamic_cast<CharType*> (v.type()));
    BoolType *styBool (dynamic_cast<BoolType*> (v.type()));

    IR::TempRegister *outReg = fun.addTempRegister(this);
    if (styInt || styChar || styBool)
    {
        if (styChar)
        {
            IR::IntType *newt (cgc.getIntType(8, false));
            styInt = newt;
            styChar = nullptr;
            IR::TempRegister *castedReg = fun.addTempRegister(newt);
            curBlock->add(std::make_unique<IR::Instrs::NoOpCast>(castedReg, v, newt));
            v = IR::ASTValue(castedReg, v.ast);
        }
        else if (styBool)
        {
            IR::IntType *newt (cgc.getIntType(1, false));
            styInt = newt;
            styBool = nullptr;
            IR::TempRegister *castedReg = fun.addTempRegister(newt);
            curBlock->add(std::make_unique<IR::Instrs::NoOpCast>(castedReg, v, newt));
            v = IR::ASTValue(castedReg, v.ast);
        }

        curBlock->add(std::make_unique<IR::Instrs::IntToInt>(outReg, v, this));
    }
    else if (styFloat)
    {
        curBlock->add(std::make_unique<IR::Instrs::FloatToInt>(outReg, v, this));
    }
    else
    {
        ERR_INVALID_CAST(ast, v, this);
    }

    return ASTValue(outReg, ast);
}
IR::ASTValue IR::IntType::implCast(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::ASTValue v)
{
    if (dynamic_cast<GenericIntType*>(v.type()))
    {
        IR::TempRegister *concreteReg = fun.addTempRegister(this);
        curBlock->add(std::make_unique<IR::Instrs::IntToInt>(concreteReg, v, this));
        return ASTValue(concreteReg, v.ast);
    }
    return v;
}
// Generic types {{{2
// GenericInt {{{2
std::string IR::GenericIntType::stringify() const
{
    return "<integer>";
}
IR::ASTValue IR::GenericIntType::binOp(BIN_OP_ARGS)
{
    ASSERT(l.type() == this);
    return intBinOp(cgc, fun, curBlock, op, l, r, optok, ast);
}
IR::ASTValue IR::GenericIntType::unaryOp(UNARY_OP_ARGS)
{
    ASSERT(v.type() == this);
    return intUnaryOp(cgc, fun, curBlock, op, v, optok, ast);
}
IR::ASTValue IR::GenericIntType::castTo(CodeGen::Context &, IR::Function &, IR::Block *&, IR::ASTValue v, ASTNS::AST *ast)
{
    ERR_INVALID_CAST(ast, v, this);
    return ASTValue();
}
llvm::Type* IR::GenericIntType::toLLVMType(llvm::LLVMContext &con) const
{
    return llvm::Type::getInt32Ty(con);
}
IR::ASTValue IR::GenericIntType::implCast(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::ASTValue v)
{
    return v;
}
// GenericFloat {{{2
std::string IR::GenericFloatType::stringify() const
{
    return "<float>";
}
IR::ASTValue IR::GenericFloatType::binOp(BIN_OP_ARGS)
{
    ASSERT(l.type() == this);
    return floatBinOp(cgc, fun, curBlock, op, l, r, optok, ast);
}
IR::ASTValue IR::GenericFloatType::unaryOp(UNARY_OP_ARGS)
{
    ASSERT(v.type() == this);
    return floatUnaryOp(cgc, fun, curBlock, op, v, optok, ast);
}
IR::ASTValue IR::GenericFloatType::castTo(CodeGen::Context &, IR::Function &, IR::Block *&, IR::ASTValue v, ASTNS::AST *ast)
{
    ERR_INVALID_CAST(ast, v, this);
    return ASTValue();
}
llvm::Type* IR::GenericFloatType::toLLVMType(llvm::LLVMContext &con) const
{
    return llvm::Type::getFloatTy(con);
}

IR::ASTValue IR::GenericFloatType::implCast(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::ASTValue v)
{
    return v;
}
// Char {{{1
llvm::Type* IR::CharType::toLLVMType(llvm::LLVMContext &con) const
{
    return llvm::Type::getInt8Ty(con);
}
std::string IR::CharType::stringify() const
{
    return "char";
}
IR::ASTValue IR::CharType::binOp(BIN_OP_ARGS)
{
    ASSERT(l.type() == this)

    l = r.type()->implCast(cgc, fun, curBlock, l);
    r = l.type()->implCast(cgc, fun, curBlock, r);
    if (l.type() != r.type())
    {
        ERR_CONFLICT_TYS_BINARY_OP(l, r, optok);
        return IR::ASTValue();
    }

    Type *retTy (defaultBinOpRetTy(cgc, l.type(), op));
    IR::TempRegister *retReg (fun.addTempRegister(retTy));

    switch (op)
    {
        SUPPORT_OPERATOR_BASIC(greater, ICmpGT)
        SUPPORT_OPERATOR_BASIC(less, ICmpLT)
        SUPPORT_OPERATOR_BASIC(greaterequal, ICmpGE)
        SUPPORT_OPERATOR_BASIC(lessequal, ICmpLE)
        SUPPORT_OPERATOR_BASIC(doubleequal, ICmpEQ)
        SUPPORT_OPERATOR_BASIC(bangequal, ICmpNE)

        default:
            ERR_LHS_UNSUPPORTED_OP(l, r, optok);
            return ASTValue();
    }

    return ASTValue(retReg, ast);
}
IR::ASTValue IR::CharType::unaryOp(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::Type::UnaryOperator op, IR::ASTValue v, Token optok, ASTNS::AST *ast)
{
    ASSERT(v.type() == this)

    ERR_UNARY_UNSUPPORTED_OP(v, optok);
    return ASTValue();
}
IR::ASTValue IR::CharType::castTo(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::ASTValue v, ASTNS::AST *ast)
{
    if (v.type() == this)
        return IR::ASTValue(v.val, ast);

    IntType *sty (dynamic_cast<IntType*>(v.type()));
    if (!sty)
    {
        ERR_INVALID_CAST(ast, v, this);
        return IR::ASTValue();
    }

    IR::IntType *charAsIntType (cgc.getIntType(8, false));
    IR::TempRegister *asIntReg = fun.addTempRegister(charAsIntType);
    curBlock->add(std::make_unique<IR::Instrs::IntToInt>(asIntReg, v, charAsIntType));

    IR::TempRegister *outReg = fun.addTempRegister(this);
    curBlock->add(std::make_unique<IR::Instrs::NoOpCast>(outReg, IR::ASTValue(asIntReg, v.ast), this));

    return ASTValue(outReg, ast);
}
IR::ASTValue IR::CharType::implCast(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::ASTValue v)
{
    return v;
}
// Bool {{{1
llvm::Type* IR::BoolType::toLLVMType(llvm::LLVMContext &con) const
{
    return llvm::Type::getInt1Ty(con);
}
std::string IR::BoolType::stringify() const
{
    return "bool";
}
IR::ASTValue IR::BoolType::binOp(CodeGen::Context &cgc, Function &fun, Block *&curBlock, BinaryOperator op, IR::ASTValue l, IR::ASTValue r, Token optok, ASTNS::AST *ast)
{
    ASSERT(l.type() == this)

    l = r.type()->implCast(cgc, fun, curBlock, l);
    r = l.type()->implCast(cgc, fun, curBlock, r);
    if (l.type() != r.type())
    {
        ERR_CONFLICT_TYS_BINARY_OP(l, r, optok);
        return IR::ASTValue();
    }

    Type *retTy (defaultBinOpRetTy(cgc, l.type(), op));
    IR::TempRegister *retReg (fun.addTempRegister(retTy));

    switch (op)
    {
        SUPPORT_OPERATOR_BASIC(amper, BitAnd)
        SUPPORT_OPERATOR_BASIC(pipe, BitOr)
        SUPPORT_OPERATOR_BASIC(caret, BitXor)
        SUPPORT_OPERATOR_BASIC(doublepipe, ShortOr)
        SUPPORT_OPERATOR_BASIC(doubleamper, ShortAnd)
        SUPPORT_OPERATOR_BASIC(doubleequal, ICmpEQ)
        SUPPORT_OPERATOR_BASIC(bangequal, ICmpNE)

        default:
            ERR_LHS_UNSUPPORTED_OP(l, r, optok);
            return ASTValue();
    }

    return ASTValue(retReg, ast);
}
IR::ASTValue IR::BoolType::unaryOp(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::Type::UnaryOperator op, IR::ASTValue v, Token optok, ASTNS::AST *ast)
{
    ASSERT(v.type() == this)

    IR::TempRegister *outReg = fun.addTempRegister(v.type());
    switch (op)
    {
        case Type::UnaryOperator::bang:
            curBlock->add(std::make_unique<IR::Instrs::Not>(outReg, v));
            break;

        default:
            ERR_UNARY_UNSUPPORTED_OP(v, optok);
            return ASTValue();
    }

    return ASTValue(outReg, ast);
}
IR::ASTValue IR::BoolType::castTo(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::ASTValue v, ASTNS::AST *ast)
{
    if (v.type() == this)
        return IR::ASTValue(v.val, ast);

    IntType *sty (dynamic_cast<IntType*>(v.type()));
    if (!sty)
    {
        ERR_INVALID_CAST(ast, v, this);
        return IR::ASTValue();
    }

    IR::IntType *boolAsIntType (cgc.getIntType(1, false));
    IR::TempRegister *asIntReg = fun.addTempRegister(boolAsIntType);
    curBlock->add(std::make_unique<IR::Instrs::IntToInt>(asIntReg, v, boolAsIntType));

    IR::TempRegister *outReg = fun.addTempRegister(this);
    curBlock->add(std::make_unique<IR::Instrs::NoOpCast>(outReg, ASTValue(asIntReg, v.ast), this));

    return ASTValue(outReg, ast);
}
IR::ASTValue IR::BoolType::implCast(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::ASTValue v)
{
    return v;
}
