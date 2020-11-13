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

Value VoidType::binOp(CodeGen::Context &, Value, Value, Token, ASTNS::AST *)
{
    fCalled("VoidType::binOp");
}
Value VoidType::unaryOp(CodeGen::Context &, Value, Token, ASTNS::AST *)
{
    fCalled("VoidType::unaryOp");
}
Value VoidType::castTo(CodeGen::Context &, Value v)
{
    Error(Error::MsgType::ERROR, v, "Invalid cast")
        .underline(Error::Underline(v, '^')
            .error(concatMsg("Invalid cast form type \"", v.type->stringify(), "\" to \"", this->stringify(), "\"")))
        .report();
    return Value();
}
Value VoidType::isTrue(CodeGen::Context &, Value)
{
    fCalled("VoidType::isTrue");
}
