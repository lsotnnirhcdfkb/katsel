#include <sstream>

#include "ir/type.h"
#include "message/errors.h"

std::string IR::VoidType::stringify()
{
    return "void";
}

bool IR::VoidType::hasOperator(TokenType)
{
    return false;
}

IR::Value* IR::VoidType::binOp(CodeGenNS::Context &, IR::Value *, IR::Value *, Token, ASTNS::AST *)
{
    fCalled("VoidType::binOp");
}
IR::Value* IR::VoidType::unaryOp(CodeGenNS::Context &, IR::Value *, Token, ASTNS::AST *)
{
    fCalled("VoidType::unaryOp");
}
IR::Value* IR::VoidType::castTo(CodeGenNS::Context &, IR::Value *v)
{
    Error(Error::MsgType::ERROR, v, "invalid cast")
        .underline(Error::Underline(v, '^')
            .error(concatMsg("invalid cast form type \"", v->type()->stringify(), "\" to \"", this->stringify(), "\"")))
        .report();
    return nullptr;
}
IR::Value* IR::VoidType::isTrue(CodeGenNS::Context &, IR::Value *)
{
    fCalled("VoidType::isTrue");
}

IR::FunctionType::FunctionType(Type *ret, std::vector<Type*> paramtys): ret(ret), paramtys(paramtys) {}

std::string IR::FunctionType::stringify()
{
    std::stringstream ss;
    ss << ret->stringify() << "(";
    for (Type *pty : paramtys)
        ss << pty->stringify() << ", ";
    ss << ")";
    return ss.str();
}

bool IR::FunctionType::hasOperator(TokenType)
{
    return false; // function has no operators
}

IR::Value* IR::FunctionType::binOp(CodeGenNS::Context &, IR::Value *, IR::Value *, Token, ASTNS::AST *)
{
    fCalled("FunctionType::binOp");
}
IR::Value* IR::FunctionType::unaryOp(CodeGenNS::Context &, IR::Value *, Token, ASTNS::AST *)
{
    fCalled("FunctionType::unaryop");
}

IR::Value* IR::FunctionType::castTo(CodeGenNS::Context &, IR::Value *v)
{
    Error(Error::MsgType::ERROR, v, "Invalid cast")
        .underline(Error::Underline(v, '^')
            .error(concatMsg("Invalid cast form type \"", v->type()->stringify(), "\" to \"", this->stringify(), "\"")))
        .report();
    return nullptr;
}

IR::Value* IR::FunctionType::isTrue(CodeGenNS::Context &, IR::Value *)
{
    fCalled("FunctionType::isTrue");
}
