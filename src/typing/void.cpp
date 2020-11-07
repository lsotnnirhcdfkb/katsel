#include <iostream> // DO NOT FORGET!!! REMOVE THIS LINE SOON!!!

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
    // msg::fCalled("VoidType::binOp");
    std::cerr << "Error: msg::fCalled(\"VoidType::binOp\");" << std::endl;
}
Value VoidType::unaryOp(CodeGenContext &, Value, Token op, ASTNS::Expr *)
{
    // msg::fCalled("VoidType::unaryOp");
    std::cerr << "Error: msg::fCalled(\"VoidType::unaryOp\");" << std::endl;
}
Value VoidType::castTo(CodeGenContext &, Value v)
{
    Error()
        .primary(Error::Primary(v)
            .error(static_cast<std::stringstream&>(std::stringstream() << "Invalid cast form type \"" << v.type->stringify() << "\" to \"" << this->stringify() << "\"").str()))
        .report();
    return Value();
}
Value VoidType::isTrue(CodeGenContext &, Value)
{
    // msg::fCalled("VoidType::isTrue");
    std::cerr << "Error: msg::fCalled(\"VoidType::isTrue\");" << std::endl;
}
Type* VoidType::pickType(Value, Value)
{
    // msg::fCalled("VoidType::pickType");
    std::cerr << "Error: msg::fCalled(\"VoidType::pickType\");" << std::endl;
}
