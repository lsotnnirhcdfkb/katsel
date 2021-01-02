#include <sstream>

#include "ir/type.h"
#include "ir/value.h"
#include "message/errmsgs.h"

#include "llvm/IR/Type.h"
#include "llvm/IR/DerivedTypes.h"
#include "utils/format.h"

IR::VoidType::VoidType(CodeGen::Context &context, ASTNS::AST *declAST): Type(context), _declAST(declAST) {}
ASTNS::AST* IR::VoidType::declAST() const {
    return _declAST;
}

std::string IR::VoidType::stringify() const {
    return "void";
}

IR::ASTValue IR::VoidType::binOp(CodeGen::Context &, IR::Function &, IR::Block *&, IR::Type::BinaryOperator , IR::ASTValue l, IR::ASTValue r, Token optok, ASTNS::AST *) {
    ERR_LHS_UNSUPPORTED_OP(l, optok);
    return ASTValue();
}
IR::ASTValue IR::VoidType::unaryOp(CodeGen::Context &, IR::Function &, IR::Block *&, IR::Type::UnaryOperator , IR::ASTValue operand, Token optok, ASTNS::AST *) {
    ERR_UNARY_UNSUPPORTED_OP(operand, optok);
    return ASTValue();
}
IR::ASTValue IR::VoidType::castTo(CodeGen::Context &, IR::Function &, IR::Block *&, IR::ASTValue v, ASTNS::AST *ast) {
    ERR_INVALID_CAST(ast, v, this);
    return ASTValue();
}

llvm::Type* IR::VoidType::toLLVMType(llvm::LLVMContext &con) const {
    return llvm::StructType::get(con);
}
IR::ASTValue IR::VoidType::implCast(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::ASTValue v) {
    return v;
}

IR::FunctionType::FunctionType(CodeGen::Context &context, ASTNS::AST *declAST, Type *ret, std::vector<Type*> paramtys): Type(context), ret(ret), paramtys(paramtys), _declAST(declAST) {}
ASTNS::AST* IR::FunctionType::declAST() const {
    return _declAST;
}

std::string IR::FunctionType::stringify() const {
    std::stringstream ss;
    ss << "fun " << ret->stringify() << "(";
    bool first = true;
    for (Type *pty : paramtys) {
        if (!first)
            ss << ", ";
        ss << pty->stringify();
        first = false;
    }
    ss << ")";
    return ss.str();
}

IR::ASTValue IR::FunctionType::binOp(CodeGen::Context &, IR::Function &, IR::Block *&, IR::Type::BinaryOperator , IR::ASTValue l, IR::ASTValue r, Token optok, ASTNS::AST *) {
    ERR_LHS_UNSUPPORTED_OP(l, optok);
    return ASTValue();
}
IR::ASTValue IR::FunctionType::unaryOp(CodeGen::Context &, IR::Function &, IR::Block *&, IR::Type::UnaryOperator , IR::ASTValue operand, Token optok, ASTNS::AST *) {
    ERR_UNARY_UNSUPPORTED_OP(operand, optok);
    return ASTValue();
}

IR::ASTValue IR::FunctionType::castTo(CodeGen::Context &, IR::Function &, IR::Block *&, IR::ASTValue v, ASTNS::AST *ast) {
    ERR_INVALID_CAST(ast, v, this);
    return ASTValue();
}

llvm::Type* IR::FunctionType::toLLVMType(llvm::LLVMContext &con) const {
    std::vector<llvm::Type*> paramsasllvm;
    for (Type *p : paramtys)
        paramsasllvm.push_back(p->toLLVMType(con));

    return llvm::FunctionType::get(ret->toLLVMType(con), paramsasllvm, false);
}
IR::ASTValue IR::FunctionType::implCast(CodeGen::Context &cgc, IR::Function &fun, IR::Block *&curBlock, IR::ASTValue v) {
    return v;
}

std::ostream& operator<<(std::ostream &os, IR::Type const *ty) {
    os << "'" << ty->stringify() << "'";
    return os;
}
