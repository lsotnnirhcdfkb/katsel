#include "typing/type.h"
#include "message/fmtmessage.h"
#include "message/errors.h"

#include "llvm/IR/Type.h"
#include "llvm/IR/DerivedTypes.h"

#include "codegen/context.h"

#include <sstream>

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
    return nullptr; // unreachable
}

llvm::Type* FunctionType::toLLVMType(llvm::LLVMContext &con)
{
    std::vector<llvm::Type*> paramsasllvm;
    for (Type *p : paramtys)
        paramsasllvm.push_back(p->toLLVMType(con));

    return llvm::FunctionType::get(ret->toLLVMType(con), paramsasllvm, false);

}

llvm::Type* VoidType::toLLVMType(llvm::LLVMContext &con)
{
    return llvm::Type::getVoidTy(con);
}

Type::~Type() {}
BuiltinType::BuiltinType(BuiltinType::Builtins b): type(b) {}
FunctionType::FunctionType(Type *ret, std::vector<Type*> paramtys): ret(ret), paramtys(paramtys) {}

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
std::string FunctionType::stringify()
{
    std::stringstream ss;
    ss << ret->stringify() << "(";
    for (Type *pty : paramtys)
        ss << pty->stringify() << ", ";
    ss << ")";
    return ss.str();
}
std::string VoidType::stringify()
{
    return "void";
}

bool BuiltinType::hasOperator(TokenType)
{
    return true; // builtin has all operators
}
bool FunctionType::hasOperator(TokenType)
{
    return false; // function has no operators
}
bool VoidType::hasOperator(TokenType)
{
    return false; // and neither does void
}

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
        // Also same TODO as below
        std::abort();

    BuiltinType *rty;
    if (!(rty = dynamic_cast<BuiltinType*>(r.type))) // if r.type is not any of builtins
    {
        reportError(op, msg::invalidROperand(l, op));
        return Value();
    }

    int ltyOrder = builtinOrder.at(this->type);
    int rtyOrder = builtinOrder.at(rty->type);

    BuiltinType *castTo;
    if (rtyOrder > ltyOrder) // lty == this
        castTo = rty; // if rty > lty, cast to rty
    else
        castTo = this; // else if rty <= lty, cast to lty

    auto castf = [&cgc] (Value v, BuiltinType *sty, BuiltinType *ety) -> Value
    {
        if (sty == ety)
            return v;
        // if not then sty < ety because you only implicitly expand types

        bool styisfloating =
            sty->type == BuiltinType::Builtins::FLOAT ||
            sty->type == BuiltinType::Builtins::DOUBLE;
        bool etyisfloating =
            ety->type == BuiltinType::Builtins::FLOAT ||
            ety->type == BuiltinType::Builtins::DOUBLE;
        bool stysigned =
            sty->type == BuiltinType::Builtins::SINT8 ||
            sty->type == BuiltinType::Builtins::SINT16 ||
            sty->type == BuiltinType::Builtins::SINT32 ||
            sty->type == BuiltinType::Builtins::SINT64 ||
            sty->type == BuiltinType::Builtins::FLOAT ||
            sty->type == BuiltinType::Builtins::CHAR ||
            sty->type == BuiltinType::Builtins::DOUBLE;
        bool etysigned =
            ety->type == BuiltinType::Builtins::SINT8 ||
            ety->type == BuiltinType::Builtins::SINT16 ||
            ety->type == BuiltinType::Builtins::SINT32 ||
            ety->type == BuiltinType::Builtins::SINT64 ||
            ety->type == BuiltinType::Builtins::FLOAT ||
            ety->type == BuiltinType::Builtins::CHAR ||
            ety->type == BuiltinType::Builtins::DOUBLE;

        if (styisfloating || etyisfloating)
        {
            // special treatment for cast to floating or cast from floating

            // cast to floating from integral -> use uitofp for unsigned integral or sitofp for signed integral
            // cast to floating from floating -> use fpext

            // cast to int from int is not in this if statement
            // cast to int from floating is impossible here becasuse that would be narrowing types

            if (!styisfloating && etyisfloating)
            {
                // cast int -> float
                // uitofp or sitofp
                if (stysigned)
                    // use uitofp
                    return Value(ety, cgc.builder.CreateUIToFP(v.val, ety->toLLVMType(cgc.context)));
                else
                    // use sitofp
                    return Value(ety, cgc.builder.CreateSIToFP(v.val, ety->toLLVMType(cgc.context)));
            }
            else if (styisfloating && etyisfloating)
            {
                // cast float -> float
                // fpext
                return Value(ety, cgc.builder.CreateFPExt(v.val, ety->toLLVMType(cgc.context)));
            }
            else
            {
                // if end type is not floating
                std::abort();
            }
        }

        // signage
        // signed -> signed : sext
        // unsigned -> signed : interpret unsigned as signed with overflow and then sext
        // unsigned -> unsigned : zext
        // signed -> unsigned : interpret sisgned as unsigned with overflow and then zext

        if (etysigned)
            // -> signed so use sext
            return Value(ety, cgc.builder.CreateSExt(v.val, ety->toLLVMType(cgc.context)));
        else
            // -> unsigned so use zext
            return Value(ety, cgc.builder.CreateZExt(v.val, ety->toLLVMType(cgc.context)));
    };

    Value lcasted = castf(l, this, castTo);
    Value rcasted = castf(r, rty, castTo);
}
Value FunctionType::binOp(CodeGenContext &, Value, Value, Token)
{
    // TODO: report internal errors
    // like
    // !!! Internal Error at __FILE__:__LINE__: <message>
    // !!! Aborting

    return Value();
}
Value VoidType::binOp(CodeGenContext &, Value, Value, Token)
{
    // Same TODO as above
    return Value();
}
