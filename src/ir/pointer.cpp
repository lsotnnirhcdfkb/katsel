#include "ir/type.h"
#include "ir/instruction.h"
#include "message/errmsgs.h"
#include "utils/format.h"
#include "utils/assert.h"

#include "codegen/codegen.h"
#include "codegen/context.h"

#include "llvm/IR/DerivedTypes.h"

IR::PointerType::PointerType(CodeGen::Context &context, NNPtr<ASTNS::AST> declAST, bool mut, NNPtr<Type> ty): Type(context), ty(ty), mut(mut), _declAST(declAST) {}
NNPtr<ASTNS::AST> IR::PointerType::declAST() const {
    return _declAST;
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

Maybe<IR::ASTValue> IR::PointerType::binOp(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &curBlock, IR::Type::BinaryOperator op, IR::ASTValue l, IR::ASTValue r, Token optok, NNPtr<ASTNS::AST> ast) {
    ASSERT(l.type().asRaw() == this);

    r = cgc.getIntType(64, true)->implCast(cgc, fun, curBlock, r);
    if (!dynamic_cast<IntType*>(r.type().asRaw())) {
        ERR_PTR_ARITH_RHS_NOT_NUM(l, optok, r);
        return Maybe<IR::ASTValue>();
    }

    switch (op) {
        case IR::Type::BinaryOperator::plus:
            return IR::ASTValue(curBlock->add(std::make_unique<IR::Instrs::PtrArith>(l, r)), ast);
        case IR::Type::BinaryOperator::minus: {
                IR::ASTValue rNegated = IR::ASTValue(curBlock->add(std::make_unique<IR::Instrs::INeg>(r)), r.ast);
                return IR::ASTValue(curBlock->add(std::make_unique<IR::Instrs::PtrArith>(l, rNegated)), ast);
            }
        case IR::Type::BinaryOperator::greater:
            return IR::ASTValue(curBlock->add(std::make_unique<IR::Instrs::ICmpGT>(l, r)), ast);
        case IR::Type::BinaryOperator::less:
            return IR::ASTValue(curBlock->add(std::make_unique<IR::Instrs::ICmpLT>(l, r)), ast);
        case IR::Type::BinaryOperator::greaterequal:
            return IR::ASTValue(curBlock->add(std::make_unique<IR::Instrs::ICmpGE>(l, r)), ast);
        case IR::Type::BinaryOperator::lessequal:
            return IR::ASTValue(curBlock->add(std::make_unique<IR::Instrs::ICmpLE>(l, r)), ast);
        case IR::Type::BinaryOperator::doubleequal:
            return IR::ASTValue(curBlock->add(std::make_unique<IR::Instrs::ICmpEQ>(l, r)), ast);
        case IR::Type::BinaryOperator::bangequal:
            return IR::ASTValue(curBlock->add(std::make_unique<IR::Instrs::ICmpNE>(l, r)), ast);

        default:
            ERR_LHS_UNSUPPORTED_OP(l, optok);
            return Maybe<IR::ASTValue>();
    }
}
Maybe<IR::ASTValue> IR::PointerType::unaryOp(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &curBlock, IR::Type::UnaryOperator op, IR::ASTValue operand, Token optok, NNPtr<ASTNS::AST> ast) {
    ASSERT(operand.type().asRaw() == this);

    ERR_UNARY_UNSUPPORTED_OP(operand, optok);
    return Maybe<ASTValue>();
}
Maybe<IR::ASTValue> IR::PointerType::castFrom(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &curBlock, IR::ASTValue v, NNPtr<ASTNS::AST> ast) {
    if (dynamic_cast<IR::PointerType*>(v.type().asRaw())) {
        return ASTValue(curBlock->add(std::make_unique<IR::Instrs::NoOpCast>(v, this)), ast);
    }

    ERR_INVALID_CAST(ast, v, this);
    return Maybe<ASTValue>();
}
NNPtr<llvm::Type> IR::PointerType::toLLVMType(llvm::LLVMContext &con) const {
    return llvm::PointerType::getUnqual(ty->toLLVMType(con).asRaw());
}
IR::ASTValue IR::PointerType::implCast(CodeGen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &curBlock, IR::ASTValue v) {
    return v;
}

