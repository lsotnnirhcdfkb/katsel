#include "typing/type.h"
#include "message/errors.h"

#include "llvm/IR/Type.h"
#include "llvm/IR/DerivedTypes.h"

#include <sstream>
#include <iostream>
#include <cstdlib>

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
    ss << "fun " << ret->stringify() << "(";
    for (Type *pty : paramtys)
        ss << pty->stringify() << ", ";
    ss << ")";
    return ss.str();
}

bool FunctionType::hasOperator(TokenType)
{
    return false; // function has no operators
}

Value FunctionType::binOp(CodeGenContext &, Value, Value, Token op, ASTNS::Expr *)
{
    // msg::fCalled("FunctionType::binOp");
    std::cerr << "Error: msg::fCalled(\"FunctionType::binOp\");" << std::endl;
}

Value FunctionType::unaryOp(CodeGenContext &, Value, Token op, ASTNS::Expr *)
{
    // msg::fCalled("FunctionType::unaryop");
    std::cerr << "Error: msg::fCalled(\"FunctionType::unaryop\");" << std::endl;
}

Value FunctionType::castTo(CodeGenContext &, Value v)
{
    // msg::invalidCast(v, this);
    std::cerr << "Error: msg::invalidCast(v, this);" << std::endl;
    return Value();
}

Value FunctionType::isTrue(CodeGenContext &, Value)
{
    // msg::fCalled("FunctionType::isTrue");
    std::cerr << "Error: msg::fCalled(\"FunctionType::isTrue\");" << std::endl;
}
Type* FunctionType::pickType(Value, Value)
{
    // msg::fCalled("FunctionType::pickType");
    std::cerr << "Error: msg::fCalled(\"FunctionType::pickType\");" << std::endl;
}
