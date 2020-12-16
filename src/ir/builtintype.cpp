#include <sstream>

#include "ir/type.h"
#include "ir/instruction.h"
#include "message/internal.h"
#include "message/errmsgs.h"

#include "codegen/codegen.h"
#include "codegen/context.h"

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
// binOp {{{1
IR::ASTValue IR::BuiltinType::binOp(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::Type::BinaryOperator op, IR::ASTValue l, IR::ASTValue r, Token optok, ASTNS::AST *ast)
{
    if (l.type() != this)
        calledWithOpTyNEthis("BuiltinType", "binOp", "left operand");

    if (l.type() != r.type())
    {
        ERR_CONFLICT_TYS_BINARY_OP(l, r, optok);
        return ASTValue();
    }

    Type *retTy;
    switch (op)
    {
        case Type::BinaryOperator::doublepipe:
        case Type::BinaryOperator::doubleamper:
        case Type::BinaryOperator::bangequal:
        case Type::BinaryOperator::doubleequal:
        case Type::BinaryOperator::less:
        case Type::BinaryOperator::greater:
        case Type::BinaryOperator::lessequal:
        case Type::BinaryOperator::greaterequal:
            retTy = cgc.getBuiltinType(BuiltinType::Builtins::BOOL);
            break;

        case Type::BinaryOperator::caret:
        case Type::BinaryOperator::pipe:
        case Type::BinaryOperator::amper:
        case Type::BinaryOperator::doubleless:
        case Type::BinaryOperator::doublegreater:
        case Type::BinaryOperator::plus:
        case Type::BinaryOperator::minus:
        case Type::BinaryOperator::star:
        case Type::BinaryOperator::slash:
        case Type::BinaryOperator::percent:
            retTy = l.type();
            break;
    }

    IR::TempRegister *outReg = fun.addTempRegister(retTy);
    switch (op)
    {
        case Type::BinaryOperator::doublepipe:
            {
                IR::Block *ltrueb = fun.addBlock("binaryor_ltrueb");
                IR::Block *checkbothb = fun.addBlock("binaryor_checkbothb");
                IR::Block *afterb = fun.addBlock("binaryor_afterb");

                // i || j
                // becomes
                // if (i)
                //     true
                //  else
                //     j

                curBlock->branch(std::make_unique<Instrs::CondBr>(l.type()->isTrue(cgc, fun, curBlock, l), ltrueb, checkbothb));

                curBlock = ltrueb;
                curBlock->branch(std::make_unique<Instrs::GotoBr>(afterb));

                curBlock = checkbothb;
                IR::ASTValue rtrue = r.type()->isTrue(cgc, fun, curBlock, r);
                curBlock->branch(std::make_unique<Instrs::GotoBr>(afterb));

                curBlock = afterb;
                curBlock->add(std::make_unique<Instrs::Phi>(outReg, std::vector {std::make_pair(ltrueb, ASTValue(cgc.getConstInt(cgc.getBuiltinType(Builtins::BOOL), 1), ast)), std::make_pair(checkbothb, rtrue)}));
            }
            break;

        case Type::BinaryOperator::doubleamper:
            {
                IR::Block *lfalseb = fun.addBlock("binaryand_lfalseb");
                IR::Block *checkbothb = fun.addBlock("binaryand_checkbothb");
                IR::Block *afterb = fun.addBlock("binaryand_afterb");

                // i && j
                // becomes
                // if (i)
                //     j
                //  else
                //     false

                curBlock->branch(std::make_unique<Instrs::CondBr>(l.type()->isTrue(cgc, fun, curBlock, l), checkbothb, lfalseb));

                curBlock = checkbothb;
                IR::ASTValue rtrue = r.type()->isTrue(cgc, fun, curBlock, r);
                curBlock->branch(std::make_unique<Instrs::GotoBr>(afterb));

                curBlock = lfalseb;
                curBlock->branch(std::make_unique<Instrs::GotoBr>(afterb));

                curBlock = afterb;
                curBlock->add(std::make_unique<Instrs::Phi>(outReg, std::vector {std::make_pair(checkbothb, rtrue), std::make_pair(lfalseb, ASTValue(cgc.getConstInt(cgc.getBuiltinType(Builtins::BOOL), 0), ast))}));
            }
            break;

        case Type::BinaryOperator::bangequal:
            curBlock->add(std::make_unique<Instrs::CmpNE>(outReg, l, r));
            break;

        case Type::BinaryOperator::doubleequal:
            curBlock->add(std::make_unique<Instrs::CmpEQ>(outReg, l, r));
            break;

        case Type::BinaryOperator::less:
            curBlock->add(std::make_unique<Instrs::CmpLT>(outReg, l, r));
            break;

        case Type::BinaryOperator::greater:
            curBlock->add(std::make_unique<Instrs::CmpGT>(outReg, l, r));
            break;

        case Type::BinaryOperator::lessequal:
            curBlock->add(std::make_unique<Instrs::CmpLE>(outReg, l, r));
            break;

        case Type::BinaryOperator::greaterequal:
            curBlock->add(std::make_unique<Instrs::CmpGE>(outReg, l, r));
            break;

        case Type::BinaryOperator::caret:
            curBlock->add(std::make_unique<Instrs::BitXor>(outReg, l, r));
            break;

        case Type::BinaryOperator::pipe:
            curBlock->add(std::make_unique<Instrs::BitOr>(outReg, l, r));
            break;

        case Type::BinaryOperator::amper:
            curBlock->add(std::make_unique<Instrs::BitAnd>(outReg, l, r));
            break;

        case Type::BinaryOperator::doubleless:
            curBlock->add(std::make_unique<Instrs::ShiftR>(outReg, l, r));
            break;

        case Type::BinaryOperator::doublegreater:
            curBlock->add(std::make_unique<Instrs::ShiftL>(outReg, l, r));
            break;

        case Type::BinaryOperator::plus:
            curBlock->add(std::make_unique<Instrs::Add>(outReg, l, r));
            break;

        case Type::BinaryOperator::minus:
            curBlock->add(std::make_unique<Instrs::Sub>(outReg, l, r));
            break;

        case Type::BinaryOperator::star:
            curBlock->add(std::make_unique<Instrs::Mult>(outReg, l, r));
            break;

        case Type::BinaryOperator::slash:
            curBlock->add(std::make_unique<Instrs::Div>(outReg, l, r));
            break;

        case Type::BinaryOperator::percent:
            curBlock->add(std::make_unique<Instrs::Mod>(outReg, l, r));
            break;
    }

    return ASTValue(outReg, ast);
}
// castTo {{{1
IR::ASTValue IR::BuiltinType::castTo(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::ASTValue v, ASTNS::AST *ast)
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

    IR::TempRegister *outReg = fun.addTempRegister(this);
    if (styintegral == etyintegral)
    {
        // int -> int
        int stySize = tysize.at(sty->type);
        int etySize = tysize.at(this->type);
        if (stySize > etySize)
            curBlock->add(std::make_unique<Instrs::Trunc>(outReg, v, this));
        else
            curBlock->add(std::make_unique<Instrs::Ext>(outReg, v, this));
    }
    else if (styintegral && !etyintegral)
    {
        // int -> fp
        curBlock->add(std::make_unique<Instrs::IntToFloat>(outReg, v, this));
    }
    else if (!styintegral && etyintegral)
    {
        // fp -> int
        curBlock->add(std::make_unique<Instrs::FloatToInt>(outReg, v, this));
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
IR::ASTValue IR::BuiltinType::unaryOp(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::Type::UnaryOperator op, IR::ASTValue v, Token optok, ASTNS::AST *ast)
{
    if (v.type() != this)
        calledWithOpTyNEthis("BuiltinType", "unaryOp", "operand");

    IR::TempRegister *outReg = fun.addTempRegister(v.type());
    switch (op)
    {
        case Type::UnaryOperator::bang:
            curBlock->add(std::make_unique<Instrs::CmpEQ>(outReg, v, ASTValue(cgc.getConstInt(this, 0), ast)));
            break;

        case Type::UnaryOperator::tilde:
            curBlock->add(std::make_unique<Instrs::BitXor>(outReg, v, ASTValue(cgc.getConstInt(this, -1), ast)));
            break;

        case Type::UnaryOperator::minus:
            curBlock->add(std::make_unique<Instrs::Sub>(outReg, ASTValue(cgc.getConstInt(this, 0), ast), v));
            break;
    }

    return ASTValue(outReg, ast);
}
// isTrue {{{1
IR::ASTValue IR::BuiltinType::isTrue(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::ASTValue v)
{
    if (v.type() != this)
        calledWithOpTyNEthis("BuiltinType", "isTrue", "value");

    IR::TempRegister *outReg = fun.addTempRegister(cgc.getBuiltinType(BuiltinType::Builtins::BOOL));
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
            curBlock->add(std::make_unique<Instrs::CmpNE>(outReg, v, ASTValue(cgc.getConstInt(this, 0), v.ast)));
            break;

        case BuiltinType::Builtins::BOOL:
            return v;
    }
    return ASTValue(outReg, v.ast);
}
// isSigned and isFloating {{{1
#define TYPEIS(t) type == Builtins::t
bool IR::BuiltinType::isSigned()
{
    return TYPEIS(SINT8) || TYPEIS(SINT16) || TYPEIS(SINT32) || TYPEIS(SINT64) || TYPEIS(FLOAT) || TYPEIS(DOUBLE) || TYPEIS(CHAR);
}
bool IR::BuiltinType::isFloating()
{
    return TYPEIS(FLOAT) || TYPEIS(DOUBLE);
}
#undef TYPEIS
