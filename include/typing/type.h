#pragma once

#include <vector>
#include <map>
#include "llvm/IR/Type.h"
#include "llvm/IR/LLVMContext.h"
class Type;
#include "value/value.h"

#include "lex/token.h"
#include "lex/tokentype.h"

class Type
{
public:
    virtual ~Type();
    virtual llvm::Type* toLLVMType(llvm::LLVMContext &l) = 0;
    virtual std::string stringify() = 0;
    virtual bool hasOperator(TokenType t) = 0;
    virtual Value binOp(Value l, Value r, Token op) = 0;
};

class BuiltinType : public Type
{
public:
    enum class Builtins
    {
        UINT8,
        UINT16,
        UINT32,
        UINT64,
        SINT8,
        SINT16,
        SINT32,
        SINT64,

        FLOAT,
        CHAR,
        BOOL,
        DOUBLE
    };
    Builtins type;

    BuiltinType(Builtins b);
    llvm::Type* toLLVMType(llvm::LLVMContext &l) override;
    std::string stringify() override;
    bool hasOperator(TokenType t) override;
    Value binOp(Value l, Value r, Token op) override;

private:
    static std::map<Builtins, int> builtinOrder;
};


class FunctionType : public Type
{
public:
    Type *ret;
    std::vector<Type*> paramtys;

    FunctionType(Type *ret, std::vector<Type*> paramtys);
    llvm::Type* toLLVMType(llvm::LLVMContext &l) override;
    std::string stringify() override;
    bool hasOperator(TokenType t) override;
    Value binOp(Value l, Value r, Token op) override;
};

class VoidType : public Type
{
public:
    llvm::Type* toLLVMType(llvm::LLVMContext &l) override;
    std::string stringify() override;
    bool hasOperator(TokenType t) override;
    Value binOp(Value l, Value r, Token op) override;
};
