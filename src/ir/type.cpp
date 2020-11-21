#include <sstream>

#include "ir/type.h"
#include "message/errors.h"

std::string VoidType::stringify()
{
    return "void";
}

bool VoidType::hasOperator(TokenType)
{
    return false;
}

Value* VoidType::binOp(CodeGenNS::Context &, Value *, Value *, Token, ASTNS::AST *)
{
    fCalled("VoidType::binOp");
}
Value* VoidType::unaryOp(CodeGenNS::Context &, Value *, Token, ASTNS::AST *)
{
    fCalled("VoidType::unaryOp");
}
Value* VoidType::castTo(CodeGenNS::Context &, Value *v)
{
    Error(Error::MsgType::ERROR, v, "invalid cast")
        .underline(Error::Underline(v, '^')
            .error(concatMsg("invalid cast form type \"", v->type()->stringify(), "\" to \"", this->stringify(), "\"")))
        .report();
    return nullptr;
}
Value* VoidType::isTrue(CodeGenNS::Context &, Value *)
{
    fCalled("VoidType::isTrue");
}

FunctionType::FunctionType(Type *ret, std::vector<Type*> paramtys): ret(ret), paramtys(paramtys) {}

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

Value* FunctionType::binOp(CodeGenNS::Context &, Value *, Value *, Token, ASTNS::AST *)
{
    fCalled("FunctionType::binOp");
}
Value* FunctionType::unaryOp(CodeGenNS::Context &, Value *, Token, ASTNS::AST *)
{
    fCalled("FunctionType::unaryop");
}

Value* FunctionType::castTo(CodeGenNS::Context &, Value *v)
{
    Error(Error::MsgType::ERROR, v, "Invalid cast")
        .underline(Error::Underline(v, '^')
            .error(concatMsg("Invalid cast form type \"", v->type()->stringify(), "\" to \"", this->stringify(), "\"")))
        .report();
    return nullptr;
}

Value* FunctionType::isTrue(CodeGenNS::Context &, Value *)
{
    fCalled("FunctionType::isTrue");
}
