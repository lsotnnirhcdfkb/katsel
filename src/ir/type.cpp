#include <sstream>

#include "ir/type.h"
#include "ir/value.h"
#include "message/errmsgs.h"

#include "llvm/IR/Type.h"
#include "llvm/IR/DerivedTypes.h"
#include "utils/format.h"

IR::VoidType::VoidType(Codegen::Context &context, Maybe<ASTNS::AST const &> decl_ast): Type(context) {
    _init_decl_ast(decl_ast);
}
std::string IR::VoidType::name() const {
    return "void";
}

Maybe<Located<NNPtr<IR::Value>>> IR::VoidType::bin_op(Codegen::Context &, IR::Function &, NNPtr<IR::Block> &, Located<ASTNS::BinaryOperator> op, Located<NNPtr<IR::Value>> l, Located<NNPtr<IR::Value>> r, ASTNS::AST const &) const {
    ERR_LHS_UNSUPPORTED_OP(l, op.span);
    return Maybe<Located<NNPtr<Value>>>();
}
Maybe<Located<NNPtr<IR::Value>>> IR::VoidType::unary_op(Codegen::Context &, IR::Function &, NNPtr<IR::Block> &, Located<ASTNS::UnaryOperator> op, Located<NNPtr<IR::Value>> operand, ASTNS::AST const &) const {
    ERR_UNARY_UNSUPPORTED_OP(operand, op);
    return Maybe<Located<NNPtr<Value>>>();
}
Maybe<Located<NNPtr<IR::Value>>> IR::VoidType::cast_from(Codegen::Context &, IR::Function &, NNPtr<IR::Block> &, Located<NNPtr<IR::Value>> v, ASTNS::AST const &ast) const {
    ERR_INVALID_CAST(ast, v, *this);
    return Maybe<Located<NNPtr<Value>>>();
}

llvm::Type& IR::VoidType::to_llvm_type(llvm::LLVMContext &con) const {
    return *llvm::StructType::get(con);
}
Located<NNPtr<IR::Value>> IR::VoidType::impl_cast(Codegen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, Located<NNPtr<IR::Value>> v) const {
    return v;
}

IR::FunctionType::FunctionType(Codegen::Context &context, Maybe<ASTNS::AST const &> decl_ast, Type const &ret, std::vector<NNPtr<Type const>> paramtys): Type(context), ret(ret), paramtys(paramtys), _decl_ast(decl_ast.has() ? Maybe<NNPtr<ASTNS::AST const>>(NNPtr(decl_ast.get())) : Maybe<NNPtr<ASTNS::AST> const>()) {
    _init_decl_ast(decl_ast);
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

Maybe<Located<NNPtr<IR::Value>>> IR::FunctionType::bin_op(Codegen::Context &, IR::Function &, NNPtr<IR::Block> &, Located<ASTNS::BinaryOperator> op, Located<NNPtr<IR::Value>> l, Located<NNPtr<IR::Value>> r, ASTNS::AST const &) const {
    ERR_LHS_UNSUPPORTED_OP(l, op.span);
    return Maybe<Located<NNPtr<Value>>>();
}
Maybe<Located<NNPtr<IR::Value>>> IR::FunctionType::unary_op(Codegen::Context &, IR::Function &, NNPtr<IR::Block> &, Located<ASTNS::UnaryOperator> op, Located<NNPtr<IR::Value>> operand, ASTNS::AST const &) const {
    ERR_UNARY_UNSUPPORTED_OP(operand, op);
    return Maybe<Located<NNPtr<Value>>>();
}

Maybe<Located<NNPtr<IR::Value>>> IR::FunctionType::cast_from(Codegen::Context &, IR::Function &, NNPtr<IR::Block> &, Located<NNPtr<IR::Value>> v, ASTNS::AST const &ast) const {
    ERR_INVALID_CAST(ast, v, *this);
    return Maybe<Located<NNPtr<Value>>>();
}

llvm::Type& IR::FunctionType::to_llvm_type(llvm::LLVMContext &con) const {
    std::vector<llvm::Type*> paramsasllvm;
    for (NNPtr<Type const> p : paramtys)
        paramsasllvm.push_back(&p->to_llvm_type(con));

    return *llvm::FunctionType::get(&ret->to_llvm_type(con), paramsasllvm, false);
}
Located<NNPtr<IR::Value>> IR::FunctionType::impl_cast(Codegen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, Located<NNPtr<IR::Value>> v) const {
    return v;
}

std::ostream& operator<<(std::ostream &os, IR::Type const &ty) {
    os << "'" << ty.name() << "'";
    return os;
}

DERIVE_DECLSYMBOL_ITEMS_IMPL(IR::VoidType)
DERIVE_DECLSYMBOL_ITEMS_IMPL(IR::FunctionType)

DERIVE_DECLSYMBOL_AST_IMPL(IR::VoidType)
DERIVE_DECLSYMBOL_AST_IMPL(IR::FunctionType)

DERIVE_TYPE_METHOD_TABLE_IMPL(IR::VoidType)
DERIVE_TYPE_METHOD_TABLE_IMPL(IR::FunctionType)

DERIVE_TYPE_NO_FIELDS(IR::VoidType)
DERIVE_TYPE_NO_FIELDS(IR::FunctionType)


#define ACCEPT(cl) void IR::cl::type_accept(IR::TypeVisitor &v) const { v.type_visit(*this); }
IR_TYPES(ACCEPT)
#undef ACCEPT
