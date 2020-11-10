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

Value FunctionType::binOp(CodeGenContext &, Value, Value, Token, ASTNS::AST *)
{
    fCalled("FunctionType::binOp");
}

Value FunctionType::unaryOp(CodeGenContext &, Value, Token, ASTNS::AST *)
{
    fCalled("FunctionType::unaryop");
}

Value FunctionType::castTo(CodeGenContext &, Value v)
{
    Error(Error::MsgType::ERROR, v, "Invalid cast")
        .primary(Error::Primary(v)
            .error(static_cast<std::stringstream&>(std::stringstream() << "Invalid cast form type \"" << v.type->stringify() << "\" to \"" << this->stringify() << "\"").str()))
        .report();
    return Value();
}

Value FunctionType::isTrue(CodeGenContext &, Value)
{
    fCalled("FunctionType::isTrue");
}
