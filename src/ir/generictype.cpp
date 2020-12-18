#include <sstream>

#include "ir/type.h"
#include "message/errmsgs.h"

#include "llvm/IR/Type.h"
#include "llvm/IR/DerivedTypes.h"

std::string IR::GenericIntType::stringify() const
{
    return "<integer>";
}
IR::ASTValue IR::GenericIntType::binOp(CodeGen::Context &, IR::Function &, IR::Block *&, IR::Type::BinaryOperator , IR::ASTValue l, IR::ASTValue r, Token optok, ASTNS::AST *)
{
    ERR_LHS_UNSUPPORTED_OP(l, r, optok);
    return ASTValue();
}
IR::ASTValue IR::GenericIntType::unaryOp(CodeGen::Context &, IR::Function &, IR::Block *&, IR::Type::UnaryOperator , IR::ASTValue operand, Token optok, ASTNS::AST *)
{
    ERR_UNARY_UNSUPPORTED_OP(operand, optok);
    return ASTValue();
}
IR::ASTValue IR::GenericIntType::castTo(CodeGen::Context &, IR::Function &, IR::Block *&, IR::ASTValue v, ASTNS::AST *asts)
{
    ERR_INVALID_CAST(ast, v, this);
    return ASTValue();
}
llvm::Type* IR::GenericIntType::toLLVMType(llvm::LLVMContext &con) const
{
    return llvm::Type::getInt32Ty(con);
}

std::string IR::GenericFloatType::stringify() const
{
    return "<float>";
}
IR::ASTValue IR::GenericFloatType::binOp(CodeGen::Context &, IR::Function &, IR::Block *&, IR::Type::BinaryOperator , IR::ASTValue l, IR::ASTValue r, Token optok, ASTNS::AST *)
{
    ERR_LHS_UNSUPPORTED_OP(l, r, optok);
    return ASTValue();
}
IR::ASTValue IR::GenericFloatType::unaryOp(CodeGen::Context &, IR::Function &, IR::Block *&, IR::Type::UnaryOperator , IR::ASTValue operand, Token optok, ASTNS::AST *)
{
    ERR_UNARY_UNSUPPORTED_OP(operand, optok);
    return ASTValue();
}
IR::ASTValue IR::GenericFloatType::castTo(CodeGen::Context &, IR::Function &, IR::Block *&, IR::ASTValue v, ASTNS::AST *asts)
{
    ERR_INVALID_CAST(ast, v, this);
    return ASTValue();
}
llvm::Type* IR::GenericFloatType::toLLVMType(llvm::LLVMContext &con) const
{
    return llvm::Type::getFloatTy(con);
}

