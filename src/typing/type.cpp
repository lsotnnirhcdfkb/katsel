#include "typing/type.h"

#include "llvm/IR/Type.h"
#include "llvm/IR/DerivedTypes.h"

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

std::map<BuiltinType::Builtins, int> BuiltinType::builtinOrder = {
    {BuiltinType::Builtins::BOOL  , 0},
    {BuiltinType::Builtins::CHAR  , 1},
    {BuiltinType::Builtins::SINT8 , 2},
    {BuiltinType::Builtins::UINT8 , 3},
    {BuiltinType::Builtins::SINT16, 4},
    {BuiltinType::Builtins::UINT16, 5},
    {BuiltinType::Builtins::SINT32, 6},
    {BuiltinType::Builtins::UINT32, 7},
    {BuiltinType::Builtins::SINT64, 8},
    {BuiltinType::Builtins::UINT64, 9},
    {BuiltinType::Builtins::FLOAT , 10},
    {BuiltinType::Builtins::DOUBLE, 11}
};

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

bool BuiltinType::hasOperator(TokenType t)
{
    return true;
}
bool FunctionType::hasOperator(TokenType)
{
    return false; // function has no operators
}
bool VoidType::hasOperator(TokenType)
{
    return false; // and neither does void
}

Value BuiltinType::binOp(Value l, Value r, Token op)
{
    if (l.type != this)
        // Also same TODO as below
        std::abort();

    std::abort();
}
Value FunctionType::binOp(Value, Value, Token)
{
    // TODO: report internal errors
    // like
    // !!! Internal Error at __FILE__:__LINE__: <message>
    // !!! Aborting

    return Value();
}
Value VoidType::binOp(Value, Value, Token)
{
    // Same TODO as above
    return Value();
}
