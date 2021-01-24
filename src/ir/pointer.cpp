#include "ir/type.h"
#include "ir/instruction.h"
#include "message/errmsgs.h"
#include "utils/format.h"
#include "utils/assert.h"

#include "codegen/codegen.h"
#include "codegen/context.h"

#include "llvm/IR/DerivedTypes.h"

IR::PointerType::PointerType(CodeGen::Context &context, NNPtr<ASTNS::AST> decl_ast, bool mut, NNPtr<Type> ty): Type(context), ty(ty), mut(mut), _decl_ast(decl_ast) {}
NNPtr<ASTNS::AST> IR::PointerType::decl_ast() const {
    return _decl_ast;
}

std::string IR::PointerType::name() const {
    if (mut)
        return format("*mut {}", ty->name());
    else
        return format("*{}", ty->name());
}

DERIVE_DECLSYMBOL_ITEMS_IMPL(IR::PointerType)
DERIVE_TYPE_METHOD_TABLE_IMPL(IR::PointerType)
DERIVE_TYPE_NO_FIELDS(IR::PointerType)

Maybe<IR::ASTValue> IR::PointerType::bin_op(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, IR::Type::BinaryOperator op, IR::ASTValue l, IR::ASTValue r, Token optok, NNPtr<ASTNS::AST> ast) {
    ASSERT(l.type().as_raw() == this);

    r = cgc.get_int_type(64, true)->impl_cast(cgc, fun, cur_block, r);
    if (!dynamic_cast<IntType*>(r.type().as_raw())) {
        ERR_PTR_ARITH_RHS_NOT_NUM(l, optok, r);
        return Maybe<IR::ASTValue>();
    }

    switch (op) {
        case IR::Type::BinaryOperator::plus:
            return IR::ASTValue(cur_block->add(std::make_unique<IR::Instrs::PtrArith>(l, r)), ast);
        case IR::Type::BinaryOperator::minus: {
                IR::ASTValue r_negated = IR::ASTValue(cur_block->add(std::make_unique<IR::Instrs::INeg>(r)), r.ast);
                return IR::ASTValue(cur_block->add(std::make_unique<IR::Instrs::PtrArith>(l, r_negated)), ast);
            }
        case IR::Type::BinaryOperator::greater:
            return IR::ASTValue(cur_block->add(std::make_unique<IR::Instrs::ICmpGT>(l, r)), ast);
        case IR::Type::BinaryOperator::less:
            return IR::ASTValue(cur_block->add(std::make_unique<IR::Instrs::ICmpLT>(l, r)), ast);
        case IR::Type::BinaryOperator::greaterequal:
            return IR::ASTValue(cur_block->add(std::make_unique<IR::Instrs::ICmpGE>(l, r)), ast);
        case IR::Type::BinaryOperator::lessequal:
            return IR::ASTValue(cur_block->add(std::make_unique<IR::Instrs::ICmpLE>(l, r)), ast);
        case IR::Type::BinaryOperator::doubleequal:
            return IR::ASTValue(cur_block->add(std::make_unique<IR::Instrs::ICmpEQ>(l, r)), ast);
        case IR::Type::BinaryOperator::bangequal:
            return IR::ASTValue(cur_block->add(std::make_unique<IR::Instrs::ICmpNE>(l, r)), ast);

        default:
            ERR_LHS_UNSUPPORTED_OP(l, optok);
            return Maybe<IR::ASTValue>();
    }
}
Maybe<IR::ASTValue> IR::PointerType::unary_op(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, IR::Type::UnaryOperator op, IR::ASTValue operand, Token optok, NNPtr<ASTNS::AST> ast) {
    ASSERT(operand.type().as_raw() == this);

    ERR_UNARY_UNSUPPORTED_OP(operand, optok);
    return Maybe<ASTValue>();
}
Maybe<IR::ASTValue> IR::PointerType::cast_from(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, IR::ASTValue v, NNPtr<ASTNS::AST> ast) {
    if (dynamic_cast<IR::PointerType*>(v.type().as_raw())) {
        return ASTValue(cur_block->add(std::make_unique<IR::Instrs::NoOpCast>(v, this)), ast);
    }

    ERR_INVALID_CAST(ast, v, this);
    return Maybe<ASTValue>();
}
NNPtr<llvm::Type> IR::PointerType::to_llvmtype(llvm::LLVMContext &con) const {
    return llvm::PointerType::getUnqual(ty->to_llvmtype(con).as_raw());
}
IR::ASTValue IR::PointerType::impl_cast(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, IR::ASTValue v) {
    return v;
}

