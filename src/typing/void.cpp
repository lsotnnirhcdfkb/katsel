#include "typing/type.h"

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

Value VoidType::binOp(CodeGenContext &, Value, Value, Token)
{
    // TODO also internal errors
    return Value();
}

Value VoidType::unaryOp(CodeGenContext &, Value, Token)
{
    return Value();
}

Value VoidType::castTo(CodeGenContext &, Value, Type *)
{
    return Value();
}
