#include "typing/type.h"

#include "llvm/IR/Type.h"
#include "llvm/IR/DerivedTypes.h"

llvm::Type* BuiltinType::toLLVMType(llvm::LLVMContext &con)
{
    switch (type)
    {
        case BuiltinType::Builtins::UINT8:
        case BuiltinType::Builtins::SINT8:
        case BuiltinType::Builtins::CHAR:
            return llvm::Type::getInt8Ty(con);

        case BuiltinType::Builtins::UINT16:
        case BuiltinType::Builtins::SINT16:
            return llvm::Type::getInt16Ty(con);

        case BuiltinType::Builtins::SINT32:
        case BuiltinType::Builtins::UINT32:
            return llvm::Type::getInt32Ty(con);

        case BuiltinType::Builtins::SINT64:
        case BuiltinType::Builtins::UINT64:
            return llvm::Type::getInt64Ty(con);

        case BuiltinType::Builtins::FLOAT:
            return llvm::Type::getFloatTy(con);
        case BuiltinType::Builtins::DOUBLE:
            return llvm::Type::getDoubleTy(con);

        case BuiltinType::Builtins::BOOL:
            return llvm::Type::getInt1Ty(con);
    }
    return nullptr; // unreachable
}

llvm::Type* FunctionType::toLLVMType(llvm::LLVMContext &con)
{
    std::vector<llvm::Type*> paramsasllvm;
    for (Type *p : paramtys)
        paramsasllvm.push_back(p->toLLVMType(con));

    return llvm::FunctionType::get(ret->toLLVMType(con), paramsasllvm, false);

}

Type::~Type() {}
BuiltinType::BuiltinType(BuiltinType::Builtins b): type(b) {}
FunctionType::FunctionType(Type *ret, std::vector<Type*> paramtys): ret(ret), paramtys(paramtys) {}

std::map<BuiltinType::Builtins, int> BuiltinType::builtinOrder = {
    {BOOL  , 0},
    {CHAR  , 1},
    {SINT8 , 2},
    {UINT8 , 3},
    {SINT16, 4},
    {UINT16, 5},
    {SINT32, 6},
    {UINT32, 7},
    {SINT64, 8},
    {UINT64, 9},
    {FLOAT , 10},
    {DOUBLE, 11}
};
