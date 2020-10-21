#include "typing/type.h"
#include "message/errors.h"

#include "llvm/IR/Type.h"

llvm::Type* VoidType::toLLVMType(llvm::LLVMContext &con)
{
    return llvm::Type::getVoidTy(con);
}

std::string VoidType::stringify()
{
    return "void";
}

bool VoidType::hasOperator(TokenType)
{
    return false;
}

Value VoidType::binOp(CodeGenContext &, Value, Value, Token op, ASTNS::Expr *)
{
    msg::fCalled("VoidType::binOp");
}
Value VoidType::unaryOp(CodeGenContext &, Value, Token op, ASTNS::Expr *)
{
    msg::fCalled("VoidType::unaryOp");
}
Value VoidType::castTo(CodeGenContext &, Value, Type *)
{
    msg::fCalled("VoidType::castTo");
}
Value VoidType::isTrue(CodeGenContext &, Value)
{
    msg::fCalled("VoidType::isTrue");
}
Type* VoidType::pickType(Value, Value)
{
    msg::fCalled("VoidType::pickType");
}
