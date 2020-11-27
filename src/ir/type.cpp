#include <sstream>

#include "ir/type.h"
#include "message/errors.h"
#include "message/errmsgs.h"

#include "llvm/IR/Type.h"
#include "llvm/IR/DerivedTypes.h"

std::string IR::VoidType::stringify() const
{
    return "void";
}

bool IR::VoidType::hasOperator(TokenType)
{
    return false;
}

IR::ASTValue IR::VoidType::binOp(CodeGenNS::Context &, IR::ASTValue , IR::ASTValue , Token, ASTNS::AST *)
{
    fCalled("VoidType::binOp");
}
IR::ASTValue IR::VoidType::unaryOp(CodeGenNS::Context &, IR::ASTValue , Token, ASTNS::AST *)
{
    fCalled("VoidType::unaryOp");
}
IR::ASTValue IR::VoidType::castTo(CodeGenNS::Context &, IR::ASTValue v, ASTNS::AST *asts)
{
    ERR_INVALID_CAST(ast, v, this);
    return ASTValue();
}
IR::ASTValue IR::VoidType::isTrue(CodeGenNS::Context &, IR::ASTValue )
{
    fCalled("VoidType::isTrue");
}

llvm::Type* IR::VoidType::toLLVMType(llvm::LLVMContext &con) const
{
    return llvm::Type::getVoidTy(con);
}

IR::FunctionType::FunctionType(Type *ret, std::vector<Type*> paramtys): ret(ret), paramtys(paramtys) {}

std::string IR::FunctionType::stringify() const
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

IR::ASTValue IR::FunctionType::binOp(CodeGenNS::Context &, IR::ASTValue , IR::ASTValue , Token, ASTNS::AST *)
{
    fCalled("FunctionType::binOp");
}
IR::ASTValue IR::FunctionType::unaryOp(CodeGenNS::Context &, IR::ASTValue , Token, ASTNS::AST *)
{
    fCalled("FunctionType::unaryop");
}

IR::ASTValue IR::FunctionType::castTo(CodeGenNS::Context &, IR::ASTValue v, ASTNS::AST *)
{
    ERR_INVALID_CAST(ast, v, this);
    return ASTValue();
}

IR::ASTValue IR::FunctionType::isTrue(CodeGenNS::Context &, IR::ASTValue )
{
    fCalled("FunctionType::isTrue");
}

llvm::Type* IR::FunctionType::toLLVMType(llvm::LLVMContext &con) const
{
    std::vector<llvm::Type*> paramsasllvm;
    for (Type *p : paramtys)
        paramsasllvm.push_back(p->toLLVMType(con));

    return llvm::FunctionType::get(ret->toLLVMType(con), paramsasllvm, false);
}
