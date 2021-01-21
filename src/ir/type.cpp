#include <sstream>

#include "ir/type.h"
#include "ir/value.h"
#include "message/errmsgs.h"

#include "llvm/IR/Type.h"
#include "llvm/IR/DerivedTypes.h"
#include "utils/format.h"

IR::VoidType::VoidType(CodeGen::Context &context, NNPtr<ASTNS::AST> declAST): Type(context), _declAST(declAST) {}
NNPtr<ASTNS::AST> IR::VoidType::declAST() const {
    return _declAST;
}
std::string IR::VoidType::name() const {
    return "void";
}

Maybe<IR::ASTValue> IR::VoidType::binOp(CodeGen::Context &, IR::Function &, NNPtr<IR::Block> &, IR::Type::BinaryOperator , IR::ASTValue l, IR::ASTValue r, Token optok, NNPtr<ASTNS::AST> ) {
    ERR_LHS_UNSUPPORTED_OP(l, optok);
    return Maybe<ASTValue>();
}
Maybe<IR::ASTValue> IR::VoidType::unaryOp(CodeGen::Context &, IR::Function &, NNPtr<IR::Block> &, IR::Type::UnaryOperator , IR::ASTValue operand, Token optok, NNPtr<ASTNS::AST> ) {
    ERR_UNARY_UNSUPPORTED_OP(operand, optok);
    return Maybe<ASTValue>();
}
Maybe<IR::ASTValue> IR::VoidType::castFrom(CodeGen::Context &, IR::Function &, NNPtr<IR::Block> &, IR::ASTValue v, NNPtr<ASTNS::AST> ast) {
    ERR_INVALID_CAST(ast, v, this);
    return Maybe<ASTValue>();
}

NNPtr<llvm::Type> IR::VoidType::toLLVMType(llvm::LLVMContext &con) const {
    return llvm::StructType::get(con);
}
IR::ASTValue IR::VoidType::implCast(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &curBlock, IR::ASTValue v) {
    return v;
}

DERIVE_TYPE_METHOD_TABLE_IMPL(IR::VoidType)
DERIVE_TYPE_METHOD_TABLE_IMPL(IR::FunctionType)

DERIVE_TYPE_NO_FIELDS(IR::VoidType)
DERIVE_TYPE_NO_FIELDS(IR::FunctionType)

IR::FunctionType::FunctionType(CodeGen::Context &context, NNPtr<ASTNS::AST> declAST, NNPtr<Type> ret, std::vector<NNPtr<Type>> paramtys): Type(context), ret(ret), paramtys(paramtys), _declAST(declAST) {}
NNPtr<ASTNS::AST> IR::FunctionType::declAST() const {
    return _declAST;
}
std::string IR::FunctionType::name() const {
    std::stringstream ss;
    ss << "fun(";
    bool first = true;
    for (NNPtr<Type> pty : paramtys) {
        if (!first)
            ss << ", ";
        ss << pty->name();
        first = false;
    }
    ss << "): " << ret->name();
    return ss.str();
}

Maybe<IR::ASTValue> IR::FunctionType::binOp(CodeGen::Context &, IR::Function &, NNPtr<IR::Block> &, IR::Type::BinaryOperator , IR::ASTValue l, IR::ASTValue r, Token optok, NNPtr<ASTNS::AST> ) {
    ERR_LHS_UNSUPPORTED_OP(l, optok);
    return Maybe<ASTValue>();
}
Maybe<IR::ASTValue> IR::FunctionType::unaryOp(CodeGen::Context &, IR::Function &, NNPtr<IR::Block> &, IR::Type::UnaryOperator , IR::ASTValue operand, Token optok, NNPtr<ASTNS::AST> ) {
    ERR_UNARY_UNSUPPORTED_OP(operand, optok);
    return Maybe<ASTValue>();
}

Maybe<IR::ASTValue> IR::FunctionType::castFrom(CodeGen::Context &, IR::Function &, NNPtr<IR::Block> &, IR::ASTValue v, NNPtr<ASTNS::AST> ast) {
    ERR_INVALID_CAST(ast, v, this);
    return Maybe<ASTValue>();
}

NNPtr<llvm::Type> IR::FunctionType::toLLVMType(llvm::LLVMContext &con) const {
    std::vector<llvm::Type*> paramsasllvm;
    for (NNPtr<Type> p : paramtys)
        paramsasllvm.push_back(p->toLLVMType(con).asRaw());

    return llvm::FunctionType::get(ret->toLLVMType(con).asRaw(), paramsasllvm, false);
}
IR::ASTValue IR::FunctionType::implCast(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &curBlock, IR::ASTValue v) {
    return v;
}

std::ostream& operator<<(std::ostream &os, NNPtr<IR::Type> const &ty) {
    os << "'" << ty->name() << "'";
    return os;
}
std::ostream& operator<<(std::ostream &os, NNPtr<IR::Type const> const &ty) {
    os << "'" << ty->name() << "'";
    return os;
}

DERIVE_DECLSYMBOL_ITEMS_IMPL(IR::VoidType)
DERIVE_DECLSYMBOL_ITEMS_IMPL(IR::FunctionType)

#define ACCEPT(cl) void IR::cl::type_accept(IR::TypeVisitor &v) { v.type_visit##cl(*this); }
IR_TYPES(ACCEPT)
#undef ACCEPT
