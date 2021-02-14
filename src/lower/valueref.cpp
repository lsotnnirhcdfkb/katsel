#include "lowererlocal.h"

using namespace Lower;

LowerValueRef::LowerValueRef(LowerFunction &fl): fl(fl) {}

llvm::Value &LowerValueRef::lower(IR::Value const &v) {
    res = nullptr;
    v.value_accept(*this);
    ASSERT(res != nullptr);
    return *res;
}

void LowerValueRef::value_visit(IR::ConstBool  const &v) { res = llvm::ConstantInt   ::get(&v.type().to_llvm_type(fl.lowerer.context), v.val); }
void LowerValueRef::value_visit(IR::ConstChar  const &v) { res = llvm::ConstantInt   ::get(&v.type().to_llvm_type(fl.lowerer.context), v.val); }
void LowerValueRef::value_visit(IR::ConstInt   const &v) { res = llvm::ConstantInt   ::get(&v.type().to_llvm_type(fl.lowerer.context), v.val); }
void LowerValueRef::value_visit(IR::ConstFloat const &v) { res = llvm::ConstantFP    ::get(&v.type().to_llvm_type(fl.lowerer.context), v.val); }
void LowerValueRef::value_visit(IR::Void       const &v) { res = llvm::ConstantStruct::get(llvm::StructType::get(fl.lowerer.context)); }

void LowerValueRef::value_visit(IR::Register const &reg) {
    res = fl.builder.CreateLoad(&fl.get_register(reg));
}
void LowerValueRef::value_visit(IR::Function const &fun) {
    res = &fl.lowerer.get_function(fun);
}
