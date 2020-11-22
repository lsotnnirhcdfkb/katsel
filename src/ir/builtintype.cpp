#include <sstream>

#include "ir/type.h"
#include "message/errors.h"

#include "codegen/codegen.h"

// Constructor {{{1
BuiltinType::BuiltinType(BuiltinType::Builtins b): type(b) {}
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
    outOSwitchNoh("BuiltinType::stringify");
}
// hasOperator {{{1
bool BuiltinType::hasOperator(TokenType)
{
    return true; // builtin has all operators
}
// binOp {{{1
Value* BuiltinType::binOp(CodeGenNS::Context &cgc, Value *l, Value *r, Token op, ASTNS::AST *ast)
{
    if (l->type() != this)
        calledWithOpTyNEthis("BuiltinType", "binOp", "left operand", l);

    if (l->type() != r->type())
    {
        Error(Error::MsgType::ERROR, op, "cannot operate on values of different types")
            .underline(Error::Underline(l, '^')
                .note(l->type()->stringify()))
            .underline(Error::Underline(r, '^')
                .note(r->type()->stringify()))
            .underline(Error::Underline(op, '-'))
            .report();
        return nullptr;
    }

    Type *retTy;
    switch (op.type)
    {
        case TokenType::DOUBLEPIPE:
        case TokenType::DOUBLEAMPER:
        case TokenType::BANGEQUAL:
        case TokenType::DOUBLEEQUAL:
        case TokenType::LESS:
        case TokenType::GREATER:
        case TokenType::LESSEQUAL:
        case TokenType::GREATEREQUAL:
            retTy = cgc.getBuiltinType(BuiltinType::Builtins::BOOL);
            break;

        case TokenType::CARET:
        case TokenType::PIPE:
        case TokenType::AMPER:
        case TokenType::DOUBLELESS:
        case TokenType::DOUBLEGREATER:
        case TokenType::PLUS:
        case TokenType::MINUS:
        case TokenType::STAR:
        case TokenType::SLASH:
        case TokenType::PERCENT:
            retTy = l->type();
            break;

        default:
            invalidTok("binary operator", op);
    }

    Register *outReg = cgc.curFunc->addRegister(retTy, ast);
    switch (op.type)
    {
        case TokenType::DOUBLEPIPE:
            cgc.curBlock->add(std::make_unique<Instrs::Or>(outReg, l, r)); // TODO: shortcircuit
            break;

        case TokenType::DOUBLEAMPER:
            cgc.curBlock->add(std::make_unique<Instrs::And>(outReg, l, r));
            break;

        case TokenType::BANGEQUAL:
            cgc.curBlock->add(std::make_unique<Instrs::IntCmpNE>(outReg, l, r));
            break;

        case TokenType::DOUBLEEQUAL:
            cgc.curBlock->add(std::make_unique<Instrs::IntCmpEQ>(outReg, l, r));
            break;

        case TokenType::LESS:
            cgc.curBlock->add(std::make_unique<Instrs::IntCmpULT>(outReg, l, r)); // TODO: unsigned and signed
            break;

        case TokenType::GREATER:
            cgc.curBlock->add(std::make_unique<Instrs::IntCmpUGT>(outReg, l, r));
            break;

        case TokenType::LESSEQUAL:
            cgc.curBlock->add(std::make_unique<Instrs::IntCmpULE>(outReg, l, r));
            break;

        case TokenType::GREATEREQUAL:
            cgc.curBlock->add(std::make_unique<Instrs::IntCmpUGE>(outReg, l, r));
            break;

        case TokenType::CARET:
            cgc.curBlock->add(std::make_unique<Instrs::BitXor>(outReg, l, r));
            break;

        case TokenType::PIPE:
            cgc.curBlock->add(std::make_unique<Instrs::BitOr>(outReg, l, r));
            break;

        case TokenType::AMPER:
            cgc.curBlock->add(std::make_unique<Instrs::BitAnd>(outReg, l, r));
            break;

        case TokenType::DOUBLELESS:
            cgc.curBlock->add(std::make_unique<Instrs::ShiftR>(outReg, l, r));
            break;

        case TokenType::DOUBLEGREATER:
            cgc.curBlock->add(std::make_unique<Instrs::ShiftL>(outReg, l, r));
            break;

        case TokenType::PLUS:
            cgc.curBlock->add(std::make_unique<Instrs::Add>(outReg, l, r));
            break;

        case TokenType::MINUS:
            cgc.curBlock->add(std::make_unique<Instrs::Sub>(outReg, l, r));
            break;

        case TokenType::STAR:
            cgc.curBlock->add(std::make_unique<Instrs::Mult>(outReg, l, r));
            break;

        case TokenType::SLASH:
            cgc.curBlock->add(std::make_unique<Instrs::Div>(outReg, l, r));
            break;

        case TokenType::PERCENT:
            cgc.curBlock->add(std::make_unique<Instrs::Mod>(outReg, l, r));

        default:
            invalidTok("binary operator", op);
    }

    return outReg;
}
// castTo {{{1
Value* BuiltinType::castTo(CodeGenNS::Context &cgc, Value *v)
{
    BuiltinType *sty = dynamic_cast<BuiltinType*> (v->type());
    if (!sty)
    {
        Error(Error::MsgType::ERROR, v, "Invalid cast")
            .underline(Error::Underline(v, '^')
                .error(concatMsg("Invalid cast from type \"", v->type()->stringify(), "\" to \"", this->stringify(), "\"")))
            .report();
        return nullptr;
    }

    if (sty == this)
        return v;

#define TYPEIS(v, e) v->type == BuiltinType::Builtins::e
    bool stysigned = TYPEIS(sty, SINT8) || TYPEIS(sty, SINT16) || TYPEIS(sty, SINT32) || TYPEIS(sty, SINT64) || TYPEIS(sty, FLOAT) || TYPEIS(sty, CHAR) || TYPEIS(sty, DOUBLE);
    bool etysigned = TYPEIS(this, SINT8) || TYPEIS(this, SINT16) || TYPEIS(this, SINT32) || TYPEIS(this, SINT64) || TYPEIS(this, FLOAT) || TYPEIS(this, CHAR) || TYPEIS(this, DOUBLE);

    bool styintegral = TYPEIS(sty, CHAR) || TYPEIS(sty, BOOL) || TYPEIS(sty, UINT8) || TYPEIS(sty, UINT16) || TYPEIS(sty, UINT32) || TYPEIS(sty, UINT64) || TYPEIS(sty, SINT8) || TYPEIS(sty, SINT16) || TYPEIS(sty, SINT32) || TYPEIS(sty, SINT64);
    bool etyintegral = TYPEIS(this, CHAR) || TYPEIS(this, BOOL) || TYPEIS(this, UINT8) || TYPEIS(this, UINT16) || TYPEIS(this, UINT32) || TYPEIS(this, UINT64) || TYPEIS(this, SINT8) || TYPEIS(this, SINT16) || TYPEIS(this, SINT32) || TYPEIS(this, SINT64);
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

    Register *outReg = cgc.curFunc->addRegister(this, v->ast());
    if (styintegral && etyintegral)
    {
        // int -> int
        int stySize = tysize.at(sty->type);
        int etySize = tysize.at(this->type);
        if (stySize > etySize)
            cgc.curBlock->add(std::make_unique<Instrs::Trunc>(outReg, v, this));
        else
            if (etysigned)
                cgc.curBlock->add(std::make_unique<Instrs::ZeroExt>(outReg, v, this));
            else
                cgc.curBlock->add(std::make_unique<Instrs::SignExt>(outReg, v, this));
    }
    else if (styintegral && !etyintegral)
    {
        // int -> fp
        if (stysigned)
            cgc.curBlock->add(std::make_unique<Instrs::SIntToFloat>(outReg, v, this));
        else
            cgc.curBlock->add(std::make_unique<Instrs::UIntToFloat>(outReg, v, this));
    }
    else if (!styintegral && etyintegral)
    {
        // fp -> int
        if (etysigned)
            cgc.curBlock->add(std::make_unique<Instrs::FloatToSInt>(outReg, v, this));
        else
            cgc.curBlock->add(std::make_unique<Instrs::FloatToUInt>(outReg, v, this));
    }
    else
    {
        // fp -> fp
        int stySize = tysize.at(sty->type);
        int etySize = tysize.at(this->type);
        if (stySize > etySize)
            cgc.curBlock->add(std::make_unique<Instrs::FloatTrunc>(outReg, v, this));
        else
            cgc.curBlock->add(std::make_unique<Instrs::FloatExt>(outReg, v, this));
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
    return outReg;
}
// unaryOp {{{1
Value* BuiltinType::unaryOp(CodeGenNS::Context &cgc, Value *v, Token op, ASTNS::AST *ast)
{
    if (v->type() != this)
        calledWithOpTyNEthis("BuiltinType", "unaryOp", "operand", v);

    Register *outReg = cgc.curFunc->addRegister(v->type(), v->ast());
    switch (op.type)
    {
        case TokenType::BANG:
            // cgc.curBlock->add(std::make_unique<Instrs::IntCmpEQ>(outReg, v, 0)); TODO: constants
            // return Value(v.type, cgc.builder.CreateICmpEQ(v.val, llvm::ConstantInt::get(v.type->toLLVMType(cgc.context), 0)), ast);

        case TokenType::TILDE:
            // cgc.curBlock->add(std::make_unique<Instrs::BitXor>(outReg, v, -1)); TODO: also constnants
            // return Value(v.type, cgc.builder.CreateXor(v.val, llvm::ConstantInt::get(v.type->toLLVMType(cgc.context), -1)), ast);

        case TokenType::MINUS:
            // cgc.curBlock->add(std::make_unique<Instrs::FloatToUInt>(outReg, v, this)); TODO: also constants
            // return Value(v.type, cgc.builder.CreateSub(llvm::ConstantInt::get(v.type->toLLVMType(cgc.context), 0), v.val), ast);

        default:
            invalidTok("unary operator", op);
    }

    return outReg;
}
// isTrue {{{1
Value* BuiltinType::isTrue(CodeGenNS::Context &cgc, Value *v)
{
    if (v->type() != this)
        calledWithOpTyNEthis("BuiltinType", "isTrue", "value", v);

    // TODO: constants
    reportAbortNoh("BuiltinType::isTrue not implemented");
    /*
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
            return Value(cgc.getBuiltinType(BuiltinType::Builtins::BOOL), cgc.builder.CreateICmpNE(v.val, llvm::ConstantInt::get(v.type->toLLVMType(cgc.context), 0)), v.ast);

        case BuiltinType::Builtins::BOOL:
            return v;

        case BuiltinType::Builtins::FLOAT:
        case BuiltinType::Builtins::DOUBLE:
            return Value(cgc.getBuiltinType(BuiltinType::Builtins::BOOL), cgc.builder.CreateFCmpONE(v.val, llvm::ConstantFP::get(v.type->toLLVMType(cgc.context), 0)), v.ast);
    }
    */
}
