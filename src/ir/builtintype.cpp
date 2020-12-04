#include <sstream>

#include "ir/type.h"
#include "ir/instruction.h"
#include "message/errors.h"
#include "message/errmsgs.h"

#include "codegen/codegen.h"

#include "llvm/IR/DerivedTypes.h"

// Constructor {{{1
IR::BuiltinType::BuiltinType(IR::BuiltinType::Builtins b): type(b) {}
// toLLVMType {{{1
llvm::Type* IR::BuiltinType::toLLVMType(llvm::LLVMContext &con) const
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
    outOSwitchNoh("BuiltinType::toLLVMType");
}
// stringify {{{1
std::string IR::BuiltinType::stringify() const
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
bool IR::BuiltinType::hasOperator(TokenType)
{
    return true; // builtin has all operators
}
// binOp {{{1
IR::ASTValue IR::BuiltinType::binOp(CodeGenNS::Context &cgc, IR::ASTValue l, IR::ASTValue r, Token op, ASTNS::AST *ast)
{
    if (l.type() != this)
        calledWithOpTyNEthis("BuiltinType", "binOp", "left operand");

    if (l.type() != r.type())
    {
        ERR_CONFLICT_TYS_BINARY_OP(l, r, op);
        return ASTValue();
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
            retTy = l.type();
            break;

        default:
            invalidTok("binary operator", op);
    }

    IR::Register *outReg = cgc.curFunc->addRegister(retTy, ast);
    switch (op.type)
    {
        case TokenType::DOUBLEPIPE:
            {
                IR::Block *ltrueb = cgc.curFunc->addBlock("binaryor_ltrueb");
                IR::Block *checkbothb = cgc.curFunc->addBlock("binaryor_checkbothb");
                IR::Block *afterb = cgc.curFunc->addBlock("binaryor_afterb");

                // i || j
                // becomes
                // if (i)
                //     true
                //  else
                //     j

                cgc.curBlock->branch(std::make_unique<Instrs::CondBr>(l, ltrueb, checkbothb));

                cgc.curBlock = ltrueb;
                cgc.curBlock->add(std::make_unique<Instrs::Store>(outReg, ASTValue(cgc.getConstInt(cgc.getBuiltinType(Builtins::BOOL), 1), ast)));
                cgc.curBlock->branch(std::make_unique<Instrs::GotoBr>(afterb));

                cgc.curBlock = checkbothb;
                cgc.curBlock->add(std::make_unique<Instrs::Store>(outReg, r.type()->isTrue(cgc, r)));
                cgc.curBlock->branch(std::make_unique<Instrs::GotoBr>(afterb));

                cgc.curBlock = afterb;
            }
            break;

        case TokenType::DOUBLEAMPER:
            {
                IR::Block *lfalseb = cgc.curFunc->addBlock("binaryand_lfalseb");
                IR::Block *checkbothb = cgc.curFunc->addBlock("binaryand_checkbothb");
                IR::Block *afterb = cgc.curFunc->addBlock("binaryand_afterb");

                // i && j
                // becomes
                // if (i)
                //     j
                //  else
                //     false

                cgc.curBlock->branch(std::make_unique<Instrs::CondBr>(l, checkbothb, lfalseb));

                cgc.curBlock = checkbothb;
                cgc.curBlock->add(std::make_unique<Instrs::Store>(outReg, r.type()->isTrue(cgc, r)));
                cgc.curBlock->branch(std::make_unique<Instrs::GotoBr>(afterb));

                cgc.curBlock = lfalseb;
                cgc.curBlock->add(std::make_unique<Instrs::Store>(outReg, ASTValue(cgc.getConstInt(cgc.getBuiltinType(Builtins::BOOL), 0), ast)));
                cgc.curBlock->branch(std::make_unique<Instrs::GotoBr>(afterb));

                cgc.curBlock = afterb;
            }
            break;

        case TokenType::BANGEQUAL:
            cgc.curBlock->add(std::make_unique<Instrs::CmpNE>(outReg, l, r));
            break;

        case TokenType::DOUBLEEQUAL:
            cgc.curBlock->add(std::make_unique<Instrs::CmpEQ>(outReg, l, r));
            break;

        case TokenType::LESS:
            cgc.curBlock->add(std::make_unique<Instrs::CmpLT>(outReg, l, r));
            break;

        case TokenType::GREATER:
            cgc.curBlock->add(std::make_unique<Instrs::CmpGT>(outReg, l, r));
            break;

        case TokenType::LESSEQUAL:
            cgc.curBlock->add(std::make_unique<Instrs::CmpLE>(outReg, l, r));
            break;

        case TokenType::GREATEREQUAL:
            cgc.curBlock->add(std::make_unique<Instrs::CmpGE>(outReg, l, r));
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
            break;

        default:
            invalidTok("binary operator", op);
    }

    return ASTValue(outReg, ast);
}
// castTo {{{1
IR::ASTValue IR::BuiltinType::castTo(CodeGenNS::Context &cgc, IR::ASTValue v, ASTNS::AST *ast)
{
    BuiltinType *sty = dynamic_cast<BuiltinType*> (v.type());
    if (!sty)
    {
        ERR_INVALID_CAST(ast, v, this);
        return IR::ASTValue();
    }

    if (sty == this)
        return v;

#define TYPEIS(v, e) v->type == BuiltinType::Builtins::e
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

    IR::Register *outReg = cgc.curFunc->addRegister(this, ast);
    if (styintegral == etyintegral)
    {
        // int -> int
        int stySize = tysize.at(sty->type);
        int etySize = tysize.at(this->type);
        if (stySize > etySize)
            cgc.curBlock->add(std::make_unique<Instrs::Trunc>(outReg, v, this));
        else
            cgc.curBlock->add(std::make_unique<Instrs::Ext>(outReg, v, this));
    }
    else if (styintegral && !etyintegral)
    {
        // int -> fp
        cgc.curBlock->add(std::make_unique<Instrs::IntToFloat>(outReg, v, this));
    }
    else if (!styintegral && etyintegral)
    {
        // fp -> int
        cgc.curBlock->add(std::make_unique<Instrs::FloatToInt>(outReg, v, this));
    }

    // From row to column cast -- this is what the code above should do
    // TODO: Turn this table into a unit test
    //          +--------+--------+--------+--------+--------+--------+--------+--------+---------+--------+--------+--------+
    //          | UINT8  | UINT16 | UINT32 | UINT64 | SINT8  | SINT16 | SINT32 | SINT64 | FLOAT   | CHAR   | BOOL   | DOUBLE |
    // +--------+--------+--------+--------+--------+--------+--------+--------+--------+---------+--------+--------+--------+
    // | UINT8  | None   | Ext    | Ext    | Ext    | None   | Ext    | Ext    | Ext    | IToF    | None   | Trunc  | IToF   |
    // +--------+--------+--------+--------+--------+--------+--------+--------+--------+---------+--------+--------+--------+
    // | UINT16 | Trunc  | None   | Ext    | Ext    | Trunc  | None   | Ext    | Ext    | IToF    | Trunc  | Trunc  | IToF   |
    // +--------+--------+--------+--------+--------+--------+--------+--------+--------+---------+--------+--------+--------+
    // | UINT32 | Trunc  | Trunc  | None   | Ext    | Trunc  | Trunc  | None   | Ext    | IToF    | Trunc  | Trunc  | IToF   |
    // +--------+--------+--------+--------+--------+--------+--------+--------+--------+---------+--------+--------+--------+
    // | UINT64 | Trunc  | Trunc  | Trunc  | None   | Trunc  | Trunc  | Trunc  | None   | IToF    | Trunc  | Trunc  | IToF   |
    // +--------+--------+--------+--------+--------+--------+--------+--------+--------+---------+--------+--------+--------+
    // | SINT8  | None   | Ext    | Ext    | Ext    | None   | Ext    | Ext    | Ext    | IToF    | None   | Trunc  | IToF   |
    // +--------+--------+--------+--------+--------+--------+--------+--------+--------+---------+--------+--------+--------+
    // | SINT16 | Trunc  | None   | Ext    | Ext    | Trunc  | None   | Ext    | Ext    | IToF    | Trunc  | Trunc  | IToF   |
    // +--------+--------+--------+--------+--------+--------+--------+--------+--------+---------+--------+--------+--------+
    // | SINT32 | Trunc  | Trunc  | None   | Ext    | Trunc  | Trunc  | None   | Ext    | IToF    | Trunc  | Trunc  | IToF   |
    // +--------+--------+--------+--------+--------+--------+--------+--------+--------+---------+--------+--------+--------+
    // | SINT64 | Trunc  | Trunc  | Trunc  | None   | Trunc  | Trunc  | Trunc  | None   | IToF    | Trunc  | Trunc  | IToF   |
    // +--------+--------+--------+--------+--------+--------+--------+--------+--------+---------+--------+--------+--------+
    // | FLOAT  | FToI   | FToI   | FToI   | FToI   | FToI   | FToI   | FToI   | FToI   | None    | FToI   | FToI   | Ext    |
    // +--------+--------+--------+--------+--------+--------+--------+--------+--------+---------+--------+--------+--------+
    // | CHAR   | None   | Ext    | Ext    | Ext    | None   | Ext    | Ext    | Ext    | IToF    | None   | Trunc  | IToF   |
    // +--------+--------+--------+--------+--------+--------+--------+--------+--------+---------+--------+--------+--------+
    // | BOOL   | Ext    | Ext    | Ext    | Ext    | Ext    | Ext    | Ext    | Ext    | IToF    | Ext    | None   | IToF   |
    // +--------+--------+--------+--------+--------+--------+--------+--------+--------+---------+--------+--------+--------+
    // | DOUBLE | FToI   | FToI   | FToI   | FToI   | FToI   | FToI   | FToI   | FToI   | Trunc   | FToI   | FToI   | None   |
    // +--------+--------+--------+--------+--------+--------+--------+--------+--------+---------+--------+--------+--------+
    return ASTValue(outReg, ast);
}
// unaryOp {{{1
IR::ASTValue IR::BuiltinType::unaryOp(CodeGenNS::Context &cgc, IR::ASTValue v, Token op, ASTNS::AST *ast)
{
    if (v.type() != this)
        calledWithOpTyNEthis("BuiltinType", "unaryOp", "operand");

    IR::Register *outReg = cgc.curFunc->addRegister(v.type(), ast);
    switch (op.type)
    {
        case TokenType::BANG:
            cgc.curBlock->add(std::make_unique<Instrs::CmpEQ>(outReg, v, ASTValue(cgc.getConstInt(this, 0), ast)));
            break;

        case TokenType::TILDE:
            cgc.curBlock->add(std::make_unique<Instrs::BitXor>(outReg, v, ASTValue(cgc.getConstInt(this, -1), ast)));
            break;

        case TokenType::MINUS:
            cgc.curBlock->add(std::make_unique<Instrs::Sub>(outReg, ASTValue(cgc.getConstInt(this, 0), ast), v));
            break;

        default:
            invalidTok("unary operator", op);
    }

    return ASTValue(outReg, ast);
}
// isTrue {{{1
IR::ASTValue IR::BuiltinType::isTrue(CodeGenNS::Context &cgc, IR::ASTValue v)
{
    if (v.type() != this)
        calledWithOpTyNEthis("BuiltinType", "isTrue", "value");

    IR::Register *outReg = cgc.curFunc->addRegister(cgc.getBuiltinType(BuiltinType::Builtins::BOOL), v.ast);
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
        case BuiltinType::Builtins::FLOAT:
        case BuiltinType::Builtins::DOUBLE:
            cgc.curBlock->add(std::make_unique<Instrs::CmpNE>(outReg, v, ASTValue(cgc.getConstInt(this, 0), v.ast)));
            break;

        case BuiltinType::Builtins::BOOL:
            return v;
    }
    return ASTValue(outReg, v.ast);
}
