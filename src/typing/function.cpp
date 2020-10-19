#include "typing/type.h"

#include "llvm/IR/Type.h"
#include "llvm/IR/DerivedTypes.h"

#include <sstream>

FunctionType::FunctionType(Type *ret, std::vector<Type*> paramtys): ret(ret), paramtys(paramtys) {}

llvm::Type* FunctionType::toLLVMType(llvm::LLVMContext &con)
{
    std::vector<llvm::Type*> paramsasllvm;
    for (Type *p : paramtys)
        paramsasllvm.push_back(p->toLLVMType(con));

    return llvm::FunctionType::get(ret->toLLVMType(con), paramsasllvm, false);

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

bool FunctionType::hasOperator(TokenType)
{
    return false; // function has no operators
}

Value FunctionType::binOp(CodeGenContext &, Value, Value, Token)
{
    // TODO: report internal errors
    // like
    // !!! Internal Error at __FILE__:__LINE__: <message>
    // !!! Aborting

    return Value();
}

Value FunctionType::unaryOp(CodeGenContext &, Value, Token)
{
    return Value();
}

Value FunctionType::castTo(CodeGenContext &, Value, Type *)
{
    return Value();
}
