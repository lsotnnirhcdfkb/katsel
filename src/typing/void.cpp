#include <sstream>

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

Value VoidType::binOp(CodeGenContext &, Value, Value, Token, ASTNS::AST *)
{
    fCalled("VoidType::binOp");
}
Value VoidType::unaryOp(CodeGenContext &, Value, Token, ASTNS::AST *)
{
    fCalled("VoidType::unaryOp");
}
Value VoidType::castTo(CodeGenContext &, Value v)
{
    Error(Error::MsgType::ERROR, v, "Invalid cast")
        .primary(Error::Primary(v)
            .error(static_cast<std::stringstream>(std::stringstream() << "Invalid cast form type \"" << v.type->stringify() << "\" to \"" << this->stringify() << "\"").str()))
        .report();
    return Value();
}
Value VoidType::isTrue(CodeGenContext &, Value)
{
    fCalled("VoidType::isTrue");
}
