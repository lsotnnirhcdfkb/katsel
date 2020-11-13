#include "typing/type.h"
#include "message/errors.h"

#include "llvm/IR/Type.h"
#include "llvm/IR/DerivedTypes.h"

#include <sstream>
#include <iostream>

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

Value FunctionType::binOp(CodeGen::Context &, Value, Value, Token, ASTNS::AST *)
{
    fCalled("FunctionType::binOp");
}

Value FunctionType::unaryOp(CodeGen::Context &, Value, Token, ASTNS::AST *)
{
    fCalled("FunctionType::unaryop");
}

Value FunctionType::castTo(CodeGen::Context &, Value v)
{
    Error(Error::MsgType::ERROR, v, "Invalid cast")
        .underline(Error::Underline(v, '^')
            .error(concatMsg("Invalid cast form type \"", v.type->stringify(), "\" to \"", this->stringify(), "\"")))
        .report();
    return Value();
}

Value FunctionType::isTrue(CodeGen::Context &, Value)
{
    fCalled("FunctionType::isTrue");
}
