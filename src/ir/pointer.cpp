#include "ir/type.h"
#include "ir/block.h"
#include "ir/function.h"
#include "ir/instruction.h"
#include "message/errmsgs.h"
#include "utils/format.h"
#include "utils/assert.h"

#include "codegen/codegen.h"
#include "codegen/context.h"

#include "llvm/IR/DerivedTypes.h"

IR::PointerType::PointerType(Codegen::Context &context, ASTNS::AST const &decl_ast, bool mut, Type const &ty): Type(context), ty(ty), mut(mut), _decl_ast(decl_ast) {}
ASTNS::AST const &IR::PointerType::decl_ast() const {
    return *_decl_ast;
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

Maybe<IR::ASTValue> IR::PointerType::bin_op(Codegen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, Located<ASTNS::BinaryOperator> op, IR::ASTValue l, IR::ASTValue r, ASTNS::AST const &ast) const {
    ASSERT(&l.type() == this);

    r = cgc.get_int_type(64, true).impl_cast(cgc, fun, cur_block, r);
    if (!dynamic_cast<IntType const *>(&r.type())) {
        ERR_PTR_ARITH_RHS_NOT_NUM(l, op, r);
        return Maybe<IR::ASTValue>();
    }

    switch (op.value) {
        case ASTNS::BinaryOperator::PLUS: {
            IR::Register &out = fun.add_register(l.type(), ast, false);
            cur_block->add<IR::Instrs::PtrArith>(out, l, r);
            return IR::ASTValue(out, ast);
        }
        case ASTNS::BinaryOperator::MINUS: {
            IR::Register &negated = fun.add_register(r.type(), *r.ast, false);
            cur_block->add<IR::Instrs::INeg>(negated, r);

            IR::Register &out = fun.add_register(l.type(), ast, false);
            cur_block->add<IR::Instrs::PtrArith>(out, l, IR::ASTValue(negated, *r.ast));
            return IR::ASTValue(out, ast);
        }
#define OP(op, instr) \
    case ASTNS::BinaryOperator::op: { \
        IR::Register &out = fun.add_register(cgc.get_bool_type(), ast, false); \
        cur_block->add<IR::Instrs::instr>(out, l, r); \
        return IR::ASTValue(out, ast); \
    }
        OP(GREATER, ICmpGT)
        OP(LESS, ICmpLT)
        OP(GREATEREQUAL, ICmpGE)
        OP(LESSEQUAL, ICmpLE)
        OP(DOUBLEEQUAL, ICmpEQ)
        OP(BANGEQUAL, ICmpNE)
#undef OP

        default:
            ERR_LHS_UNSUPPORTED_OP(l, op);
            return Maybe<IR::ASTValue>();
    }
}
Maybe<IR::ASTValue> IR::PointerType::unary_op(Codegen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, Located<ASTNS::UnaryOperator> op, IR::ASTValue operand, ASTNS::AST const &ast) const {
    ASSERT(&operand.type() == this);

    ERR_UNARY_UNSUPPORTED_OP(operand, op);
    return Maybe<ASTValue>();
}
Maybe<IR::ASTValue> IR::PointerType::cast_from(Codegen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, IR::ASTValue v, ASTNS::AST const &ast) const {
    if (dynamic_cast<IR::PointerType const *>(&v.type())) {
        IR::Register &out = fun.add_register(*this, *v.ast, false);
        cur_block->add<IR::Instrs::NoOpCast>(out, v, this);
        return IR::ASTValue(out, ast);
    }

    ERR_INVALID_CAST(ast, v, *this);
    return Maybe<ASTValue>();
}
llvm::Type& IR::PointerType::to_llvmtype(llvm::LLVMContext &con) const {
    return *llvm::PointerType::getUnqual(&ty->to_llvmtype(con));
}
IR::ASTValue IR::PointerType::impl_cast(Codegen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, IR::ASTValue v) const {
    return v;
}

