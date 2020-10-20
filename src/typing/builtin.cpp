#include "typing/type.h"
#include "message/fmtmessage.h"
#include "message/errors.h"

#include "llvm/IR/Type.h"

#include "codegen/context.h"

// Constructor {{{1
BuiltinType::BuiltinType(BuiltinType::Builtins b): type(b) {}
// toLLVMType {{{1
llvm::Type* BuiltinType::toLLVMType(llvm::LLVMContext &con)
{
    switch (type)
    {
        case BuiltinType::Builtins::UINT8:
        case BuiltinType::Builtins::SINT8:
        case BuiltinType::Builtins::CHAR:
            return llvm::Type::getInt8Ty(con);

        case BuiltinType::Builtins::UINT16:
        case BuiltinType::Builtins::SINT16:
            return llvm::Type::getInt16Ty(con);

        case BuiltinType::Builtins::SINT32:
        case BuiltinType::Builtins::UINT32:
            return llvm::Type::getInt32Ty(con);

        case BuiltinType::Builtins::SINT64:
        case BuiltinType::Builtins::UINT64:
            return llvm::Type::getInt64Ty(con);

        case BuiltinType::Builtins::FLOAT:
            return llvm::Type::getFloatTy(con);
        case BuiltinType::Builtins::DOUBLE:
            return llvm::Type::getDoubleTy(con);

        case BuiltinType::Builtins::BOOL:
            return llvm::Type::getInt1Ty(con);
    }
    std::cerr << "BultinType::toLLVMType went out of switch" << std::endl;
    std::abort();
}
// stringify {{{1
std::string BuiltinType::stringify()
{
    switch (type)
    {
        case BuiltinType::Builtins::UINT8: return "uint8";
        case BuiltinType::Builtins::UINT16: return "uint16";
        case BuiltinType::Builtins::UINT32: return "uint32";
        case BuiltinType::Builtins::UINT64: return "uint64";
        case BuiltinType::Builtins::SINT8: return "sint8";
        case BuiltinType::Builtins::SINT16: return "sint16";
        case BuiltinType::Builtins::SINT32: return "sint32";
        case BuiltinType::Builtins::SINT64: return "sint64";

        case BuiltinType::Builtins::FLOAT: return "float";
        case BuiltinType::Builtins::CHAR: return "char";
        case BuiltinType::Builtins::BOOL: return "bool";
        case BuiltinType::Builtins::DOUBLE: return "double";
    }
}
// hasOperator {{{1
bool BuiltinType::hasOperator(TokenType)
{
    return true; // builtin has all operators
}
// binOp {{{1
Value BuiltinType::binOp(CodeGenContext &cgc, Value l, Value r, Token op)
{
    const static std::map<BuiltinType::Builtins, int> builtinOrder = {
        {BuiltinType::Builtins::BOOL  ,  0},
        {BuiltinType::Builtins::CHAR  ,  1},
        {BuiltinType::Builtins::SINT8 ,  1},
        {BuiltinType::Builtins::UINT8 ,  2},
        {BuiltinType::Builtins::SINT16,  3},
        {BuiltinType::Builtins::UINT16,  4},
        {BuiltinType::Builtins::SINT32,  5},
        {BuiltinType::Builtins::UINT32,  6},
        {BuiltinType::Builtins::SINT64,  7},
        {BuiltinType::Builtins::UINT64,  8},
        {BuiltinType::Builtins::FLOAT ,  9},
        {BuiltinType::Builtins::DOUBLE, 10}
    };

    if (l.type != this)
        report(MsgType::INTERNALERR, "BuiltinType::binOp called with l value type not equal to this", op, op);

    BuiltinType *rty;
    if (!(rty = dynamic_cast<BuiltinType*>(r.type))) // if r.type is not any of builtins
    {
        report(MsgType::ERROR, msg::invalidROperand(l, op), op);
        return Value();
    }

    int ltyOrder = builtinOrder.at(this->type);
    int rtyOrder = builtinOrder.at(rty->type);

    BuiltinType *destTy;
    if (rtyOrder > ltyOrder) // lty == this
        destTy = rty; // if rty > lty, cast to rty
    else
        destTy = this; // else if rty <= lty, cast to lty

    Value lcasted = castTo(cgc, l, destTy);
    Value rcasted = castTo(cgc, r, destTy);

    switch (op.type)
    {
        case TokenType::DOUBLEPIPE:
            return Value(cgc.getBuiltinType(BuiltinType::Builtins::BOOL), cgc.builder.CreateOr(lcasted.val, rcasted.val)); // TODO: Shortcircuit
            break;

        case TokenType::DOUBLEAMPER:
            return Value(cgc.getBuiltinType(BuiltinType::Builtins::BOOL), cgc.builder.CreateAnd(lcasted.val, rcasted.val));
            break;

        case TokenType::BANGEQUAL:
            return Value(cgc.getBuiltinType(BuiltinType::Builtins::BOOL), cgc.builder.CreateICmpNE(lcasted.val, rcasted.val));
            break;

        case TokenType::DOUBLEEQUAL:
            return Value(cgc.getBuiltinType(BuiltinType::Builtins::BOOL), cgc.builder.CreateICmpEQ(lcasted.val, rcasted.val));
            break;

        case TokenType::LESS:
            return Value(cgc.getBuiltinType(BuiltinType::Builtins::BOOL), cgc.builder.CreateICmpULT(lcasted.val, rcasted.val)); // TODO: unsigned and signed
            break;

        case TokenType::GREATER:
            return Value(cgc.getBuiltinType(BuiltinType::Builtins::BOOL), cgc.builder.CreateICmpUGT(lcasted.val, rcasted.val));
            break;

        case TokenType::LESSEQUAL:
            return Value(cgc.getBuiltinType(BuiltinType::Builtins::BOOL), cgc.builder.CreateICmpULE(lcasted.val, rcasted.val));
            break;

        case TokenType::GREATEREQUAL:
            return Value(cgc.getBuiltinType(BuiltinType::Builtins::BOOL), cgc.builder.CreateICmpUGE(lcasted.val, rcasted.val));
            break;

        case TokenType::CARET:
            return Value(destTy, cgc.builder.CreateXor(lcasted.val, rcasted.val));
            break;

        case TokenType::PIPE:
            return Value(destTy, cgc.builder.CreateOr(lcasted.val, rcasted.val));
            break;

        case TokenType::AMPER:
            return Value(destTy, cgc.builder.CreateAnd(lcasted.val, rcasted.val));
            break;

        case TokenType::DOUBLELESS:
            return Value(destTy, cgc.builder.CreateShl(lcasted.val, rcasted.val));
            break;

        case TokenType::DOUBLEGREATER:
            return Value(destTy, cgc.builder.CreateLShr(lcasted.val, rcasted.val));
            break;

        case TokenType::PLUS:
            return Value(destTy, cgc.builder.CreateAdd(lcasted.val, rcasted.val));
            break;

        case TokenType::MINUS:
            return Value(destTy, cgc.builder.CreateSub(lcasted.val, rcasted.val));
            break;

        case TokenType::STAR:
            return Value(destTy, cgc.builder.CreateMul(lcasted.val, rcasted.val));
            break;

        case TokenType::SLASH:
            return Value(destTy, cgc.builder.CreateUDiv(lcasted.val, rcasted.val));
            break;

        case TokenType::PERCENT:
            return Value(destTy, cgc.builder.CreateURem(lcasted.val, rcasted.val));
            break;

        default:
            report(MsgType::INTERNALERR, "Invalid binary operator", op, op);
    }

    report(MsgType::INTERNALERR, "binOp went out of switch despite default label", op, op);
    return Value(); // literally unreachable
}
// castTo {{{1
Value BuiltinType::castTo(CodeGenContext &cgc, Value v, Type *toty)
{
    BuiltinType *sty = dynamic_cast<BuiltinType*> (v.type);
    BuiltinType *ety = dynamic_cast<BuiltinType*> (toty);
    if (!sty || !ety)
    {
        // reportError(MsgType::ERROR, msg::invalidCast(v.type, toty));
        std::cerr << msg::invalidCast(v.type, toty) << std::endl; // TODO: fix this
        return Value();
    }

    if (sty == ety)
        return v;

#define TYPEIS(v, e) v->type == BuiltinType::Builtins::e
    bool stysigned = TYPEIS(sty, SINT8) || TYPEIS(sty, SINT16) || TYPEIS(sty, SINT32) || TYPEIS(sty, SINT64) || TYPEIS(sty, FLOAT) || TYPEIS(sty, CHAR) || TYPEIS(sty, DOUBLE);
    bool etysigned = TYPEIS(ety, SINT8) || TYPEIS(ety, SINT16) || TYPEIS(ety, SINT32) || TYPEIS(ety, SINT64) || TYPEIS(ety, FLOAT) || TYPEIS(ety, CHAR) || TYPEIS(ety, DOUBLE);

    bool styintegral = TYPEIS(sty, CHAR) || TYPEIS(sty, BOOL) || TYPEIS(sty, UINT8) || TYPEIS(sty, UINT16) || TYPEIS(sty, UINT32) || TYPEIS(sty, UINT64) || TYPEIS(sty, SINT8) || TYPEIS(sty, SINT16) || TYPEIS(sty, SINT32) || TYPEIS(sty, SINT64);
    bool etyintegral = TYPEIS(ety, CHAR) || TYPEIS(ety, BOOL) || TYPEIS(ety, UINT8) || TYPEIS(ety, UINT16) || TYPEIS(ety, UINT32) || TYPEIS(ety, UINT64) || TYPEIS(ety, SINT8) || TYPEIS(ety, SINT16) || TYPEIS(ety, SINT32) || TYPEIS(ety, SINT64);
#undef TYPEIS

    const static std::map<BuiltinType::Builtins, int> tysize = {
        {BuiltinType::Builtins::UINT8 , 8},
        {BuiltinType::Builtins::UINT16, 16},
        {BuiltinType::Builtins::UINT32, 32},
        {BuiltinType::Builtins::UINT64, 64},
        {BuiltinType::Builtins::SINT8 , 8},
        {BuiltinType::Builtins::SINT16, 16},
        {BuiltinType::Builtins::SINT32, 32},
        {BuiltinType::Builtins::SINT64, 64},
        {BuiltinType::Builtins::FLOAT , 32},
        {BuiltinType::Builtins::CHAR  , 8},
        {BuiltinType::Builtins::BOOL  , 1},
        {BuiltinType::Builtins::DOUBLE, 64}
    };

    if (styintegral && etyintegral)
    {
        // int -> int
        int stySize = tysize.at(sty->type);
        int etySize = tysize.at(ety->type);
        if (stySize > etySize)
            return Value(ety, cgc.builder.CreateTrunc(v.val, ety->toLLVMType(cgc.context)));
        else
            if (etysigned)
                return Value(ety, cgc.builder.CreateZExt(v.val, ety->toLLVMType(cgc.context)));
            else
                return Value(ety, cgc.builder.CreateSExt(v.val, ety->toLLVMType(cgc.context)));
    }
    else if (styintegral && !etyintegral)
    {
        // int -> fp
        if (stysigned)
            return Value(ety, cgc.builder.CreateSIToFP(v.val, ety->toLLVMType(cgc.context)));
        else
            return Value(ety, cgc.builder.CreateUIToFP(v.val, ety->toLLVMType(cgc.context)));
    }
    else if (!styintegral && etyintegral)
    {
        // fp -> int
        if (etysigned)
            return Value(ety, cgc.builder.CreateFPToSI(v.val, ety->toLLVMType(cgc.context)));
        else
            return Value(ety, cgc.builder.CreateFPToUI(v.val, ety->toLLVMType(cgc.context)));
    }
    else
    {
        // fp -> fp
        int stySize = tysize.at(sty->type);
        int etySize = tysize.at(ety->type);
        if (stySize > etySize)
            return Value(ety, cgc.builder.CreateFPTrunc(v.val, ety->toLLVMType(cgc.context)));
        else
            return Value(ety, cgc.builder.CreateFPExt(v.val, ety->toLLVMType(cgc.context)));
    }

    // From row to column cast -- this is what the code above should do
    // TODO: Turn this table into a unit test
    //          +--------+--------+--------+--------+--------+--------+--------+--------+---------+--------+--------+--------+
    //          | UINT8  | UINT16 | UINT32 | UINT64 | SINT8  | SINT16 | SINT32 | SINT64 | FLOAT   | CHAR   | BOOL   | DOUBLE |
    // +--------+--------+--------+--------+--------+--------+--------+--------+--------+---------+--------+--------+--------+
    // | UINT8  | None   | ZExt   | ZExt   | ZExt   | None   | SExt   | SExt   | SExt   | UIToFP  | None   | Trunc  | UIToFP |
    // +--------+--------+--------+--------+--------+--------+--------+--------+--------+---------+--------+--------+--------+
    // | UINT16 | Trunc  | None   | ZExt   | ZExt   | Trunc  | None   | SExt   | SExt   | UIToFP  | Trunc  | Trunc  | UIToFP |
    // +--------+--------+--------+--------+--------+--------+--------+--------+--------+---------+--------+--------+--------+
    // | UINT32 | Trunc  | Trunc  | None   | ZExt   | Trunc  | Trunc  | None   | SExt   | UIToFP  | Trunc  | Trunc  | UIToFP |
    // +--------+--------+--------+--------+--------+--------+--------+--------+--------+---------+--------+--------+--------+
    // | UINT64 | Trunc  | Trunc  | Trunc  | None   | Trunc  | Trunc  | Trunc  | None   | UIToFP  | Trunc  | Trunc  | UIToFP |
    // +--------+--------+--------+--------+--------+--------+--------+--------+--------+---------+--------+--------+--------+
    // | SINT8  | None   | ZExt   | ZExt   | ZExt   | None   | SExt   | SExt   | SExt   | SIToFP  | None   | Trunc  | SIToFP |
    // +--------+--------+--------+--------+--------+--------+--------+--------+--------+---------+--------+--------+--------+
    // | SINT16 | Trunc  | None   | ZExt   | ZExt   | Trunc  | None   | SExt   | SExt   | SIToFP  | Trunc  | Trunc  | SIToFP |
    // +--------+--------+--------+--------+--------+--------+--------+--------+--------+---------+--------+--------+--------+
    // | SINT32 | Trunc  | Trunc  | None   | ZExt   | Trunc  | Trunc  | None   | SExt   | SIToFP  | Trunc  | Trunc  | SIToFP |
    // +--------+--------+--------+--------+--------+--------+--------+--------+--------+---------+--------+--------+--------+
    // | SINT64 | Trunc  | Trunc  | Trunc  | None   | Trunc  | Trunc  | Trunc  | None   | SIToFP  | Trunc  | Trunc  | SIToFP |
    // +--------+--------+--------+--------+--------+--------+--------+--------+--------+---------+--------+--------+--------+
    // | FLOAT  | FPToUI | FPToUI | FPToUI | FPToUI | FPToSI | FPToSI | FPToSI | FPToSI | None    | FPToUI | FPToUI | FPExt  |
    // +--------+--------+--------+--------+--------+--------+--------+--------+--------+---------+--------+--------+--------+
    // | CHAR   | None   | ZExt   | ZExt   | ZExt   | None   | SExt   | SExt   | SExt   | SIToFP  | None   | Trunc  | SIToFP |
    // +--------+--------+--------+--------+--------+--------+--------+--------+--------+---------+--------+--------+--------+
    // | BOOL   | ZExt   | ZExt   | ZExt   | ZExt   | SExt   | SExt   | SExt   | SExt   | UIToFP  | ZExt   | None   | UIToFP |
    // +--------+--------+--------+--------+--------+--------+--------+--------+--------+---------+--------+--------+--------+
    // | DOUBLE | FPToUI | FPToUI | FPToUI | FPToUI | FPToSI | FPToSI | FPToSI | FPToSI | FPTrunc | FPToSI | FPToUI | None   |
    // +--------+--------+--------+--------+--------+--------+--------+--------+--------+---------+--------+--------+--------+
}
// unaryOp {{{1
Value BuiltinType::unaryOp(CodeGenContext &cgc, Value v, Token op)
{
    if (v.type != this)
        report(MsgType::INTERNALERR, "BuiltinType::unaryOp called with operand type not equal to this", op, op);

    switch (op.type)
    {
        case TokenType::BANG:
            return Value(v.type, cgc.builder.CreateICmpEQ(v.val, llvm::ConstantInt::get(v.type->toLLVMType(cgc.context), 0)));
            break;

        case TokenType::TILDE:
            return Value(v.type, cgc.builder.CreateXor(v.val, llvm::ConstantInt::get(v.type->toLLVMType(cgc.context), -1)));
            break;

        case TokenType::MINUS:
            return Value(v.type, cgc.builder.CreateSub(llvm::ConstantInt::get(v.type->toLLVMType(cgc.context), 0), v.val));
            break;

        default:
            report(MsgType::INTERNALERR, "Invalid unary operator", op, op);
    }

    report(MsgType::INTERNALERR, "unaryOp went out of switch despite default label", op, op);
    return Value(); // literally unreachable
}
// isTrue {{{1
Value BuiltinType::isTrue(CodeGenContext &cgc, Value v)
{
    if (v.type != this)
    {
        std::cerr << "BuiltinType::isTrue called with value type not equal to this" << std::endl;
        std::abort();
        // report(MsgType::INTERNALERR, "BuiltinType::isTrue called with value type not equal to this"); TODO: make this work somehow
    }

    switch (type)
    {
        case BuiltinType::Builtins::UINT8:
        case BuiltinType::Builtins::UINT16:
        case BuiltinType::Builtins::UINT32:
        case BuiltinType::Builtins::UINT64:
        case BuiltinType::Builtins::SINT8:
        case BuiltinType::Builtins::SINT16:
        case BuiltinType::Builtins::SINT32:
        case BuiltinType::Builtins::SINT64:
        case BuiltinType::Builtins::CHAR:
            return Value(cgc.getBuiltinType(BuiltinType::Builtins::BOOL), cgc.builder.CreateICmpNE(v.val, llvm::ConstantInt::get(v.type->toLLVMType(cgc.context), 0)));

        case BuiltinType::Builtins::BOOL:
            return v;

        case BuiltinType::Builtins::FLOAT:
        case BuiltinType::Builtins::DOUBLE:
            return Value(cgc.getBuiltinType(BuiltinType::Builtins::BOOL), cgc.builder.CreateFCmpONE(v.val, llvm::ConstantFP::get(v.type->toLLVMType(cgc.context), 0)));
    }
}
