#include <sstream>

#include "ir/type.h"
#include "ir/value.h"
#include "message/errmsgs.h"

#include "llvm/IR/Type.h"
#include "llvm/IR/DerivedTypes.h"
#include "utils/format.h"

IR::VoidType::VoidType(CodeGen::Context &context, ASTNS::AST const &decl_ast): Type(context), _decl_ast(decl_ast) {}
ASTNS::AST const &IR::VoidType::decl_ast() const {
    return *_decl_ast;
}
std::string IR::VoidType::name() const {
    return "void";
}

Maybe<IR::ASTValue> IR::VoidType::bin_op(CodeGen::Context &, IR::Function &, NNPtr<IR::Block> &, Located<ASTNS::BinaryOperator> op, IR::ASTValue l, IR::ASTValue r, ASTNS::AST const &) const {
    ERR_LHS_UNSUPPORTED_OP(l, op);
    return Maybe<ASTValue>();
}
Maybe<IR::ASTValue> IR::VoidType::unary_op(CodeGen::Context &, IR::Function &, NNPtr<IR::Block> &, Located<ASTNS::UnaryOperator> op, IR::ASTValue operand, ASTNS::AST const &) const {
    ERR_UNARY_UNSUPPORTED_OP(operand, op);
    return Maybe<ASTValue>();
}
Maybe<IR::ASTValue> IR::VoidType::cast_from(CodeGen::Context &, IR::Function &, NNPtr<IR::Block> &, IR::ASTValue v, ASTNS::AST const &ast) const {
    ERR_INVALID_CAST(ast, v, *this);
    return Maybe<ASTValue>();
}

llvm::Type& IR::VoidType::to_llvmtype(llvm::LLVMContext &con) const {
    return *llvm::StructType::get(con);
}
IR::ASTValue IR::VoidType::impl_cast(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, IR::ASTValue v) const {
    return v;
}

DERIVE_TYPE_METHOD_TABLE_IMPL(IR::VoidType)
DERIVE_TYPE_METHOD_TABLE_IMPL(IR::FunctionType)

DERIVE_TYPE_NO_FIELDS(IR::VoidType)
DERIVE_TYPE_NO_FIELDS(IR::FunctionType)

IR::FunctionType::FunctionType(CodeGen::Context &context, ASTNS::AST const &decl_ast, Type const &ret, std::vector<NNPtr<Type const>> paramtys): Type(context), ret(ret), paramtys(paramtys), _decl_ast(decl_ast) {}
ASTNS::AST const &IR::FunctionType::decl_ast() const {
    return *_decl_ast;
}
std::string IR::FunctionType::name() const {
    std::stringstream ss;
    ss << "fun(";
    bool first = true;
    for (NNPtr<Type const> pty : paramtys) {
        if (!first)
            ss << ", ";
        ss << pty->name();
        first = false;
    }
    ss << "): " << ret->name();
    return ss.str();
}

Maybe<IR::ASTValue> IR::FunctionType::bin_op(CodeGen::Context &, IR::Function &, NNPtr<IR::Block> &, Located<ASTNS::BinaryOperator> op, IR::ASTValue l, IR::ASTValue r, ASTNS::AST const &) const {
    ERR_LHS_UNSUPPORTED_OP(l, op);
    return Maybe<ASTValue>();
}
Maybe<IR::ASTValue> IR::FunctionType::unary_op(CodeGen::Context &, IR::Function &, NNPtr<IR::Block> &, Located<ASTNS::UnaryOperator> op, IR::ASTValue operand, ASTNS::AST const &) const {
    ERR_UNARY_UNSUPPORTED_OP(operand, op);
    return Maybe<ASTValue>();
}

Maybe<IR::ASTValue> IR::FunctionType::cast_from(CodeGen::Context &, IR::Function &, NNPtr<IR::Block> &, IR::ASTValue v, ASTNS::AST const &ast) const {
    ERR_INVALID_CAST(ast, v, *this);
    return Maybe<ASTValue>();
}

llvm::Type& IR::FunctionType::to_llvmtype(llvm::LLVMContext &con) const {
    std::vector<llvm::Type*> paramsasllvm;
    for (NNPtr<Type const> p : paramtys)
        paramsasllvm.push_back(&p->to_llvmtype(con));

    return *llvm::FunctionType::get(&ret->to_llvmtype(con), paramsasllvm, false);
}
IR::ASTValue IR::FunctionType::impl_cast(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, IR::ASTValue v) const {
    return v;
}

std::ostream& operator<<(std::ostream &os, IR::Type const &ty) {
    os << "'" << ty.name() << "'";
    return os;
}

DERIVE_DECLSYMBOL_ITEMS_IMPL(IR::VoidType)
DERIVE_DECLSYMBOL_ITEMS_IMPL(IR::FunctionType)

#define ACCEPT(cl) void IR::cl::type_accept(IR::TypeVisitor &v) const { v.type_visit(*this); }
IR_TYPES(ACCEPT)
#undef ACCEPT
