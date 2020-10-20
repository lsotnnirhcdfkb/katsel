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

Value VoidType::binOp(CodeGenContext &, Value, Value, Token op)
{
    report(MsgType::INTERNALERR, "VoidType::binOp called", op, op);
    return Value();
}

Value VoidType::unaryOp(CodeGenContext &, Value, Token op)
{
    report(MsgType::INTERNALERR, "VoidType::unaryOp called", op, op);
    return Value();
}

Value VoidType::castTo(CodeGenContext &, Value, Type *)
{
    std::cerr << "VoidType::castTo called" << std::endl; // TODO: this work
    std::abort();
}

Value VoidType::isTrue(CodeGenContext &, Value)
{
    return Value();
}
void VoidType::castTwoVals(CodeGenContext &, Value &, Value &)
{
    std::cerr << "VoidType::castTwoVals called" << std::endl;
    std::abort();
}
