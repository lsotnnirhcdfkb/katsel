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

Maybe<Located<NNPtr<IR::Value>>> IR::PointerType::bin_op(Codegen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, Located<ASTNS::BinaryOperator> op, Located<NNPtr<IR::Value>> l, Located<NNPtr<IR::Value>> r, ASTNS::AST const &ast) const {
    ASSERT(&l.value->type() == this);

    r = cgc.get_int_type(64, true).impl_cast(cgc, fun, cur_block, r);
    if (!dynamic_cast<IntType const *>(&r.value->type())) {
        ERR_PTR_ARITH_RHS_NOT_NUM(l, op, r);
        return Maybe<Located<NNPtr<IR::Value>>>();
    }

    switch (op.value) {
        case ASTNS::BinaryOperator::PLUS: {
            IR::Register &out = fun.add_register(l.value->type(), ast, false);
            cur_block->add<IR::Instrs::PtrArith>(out, l, r);
            return Located<NNPtr<IR::Value>> { ast, out };
        }
        case ASTNS::BinaryOperator::MINUS: {
            IR::Register &negated = fun.add_register(r.value->type(), r.span, false);
            cur_block->add<IR::Instrs::INeg>(negated, r);

            IR::Register &out = fun.add_register(l.value->type(), ast, false);
            cur_block->add<IR::Instrs::PtrArith>(out, l, Located<NNPtr<IR::Value>> { r.span, negated });
            return Located<NNPtr<IR::Value>> { ast, out };
        }
#define OP(op, instr) \
    case ASTNS::BinaryOperator::op: { \
        IR::Register &out = fun.add_register(cgc.get_bool_type(), ast, false); \
        cur_block->add<IR::Instrs::instr>(out, l, r); \
        return Located<NNPtr<IR::Value>> { ast, out }; \
    }
        OP(GREATER, ICmpGT)
        OP(LESS, ICmpLT)
        OP(GREATEREQUAL, ICmpGE)
        OP(LESSEQUAL, ICmpLE)
        OP(DOUBLEEQUAL, ICmpEQ)
        OP(BANGEQUAL, ICmpNE)
#undef OP

        default:
            ERR_LHS_UNSUPPORTED_OP(l, op.span);
            return Maybe<Located<NNPtr<IR::Value>>>();
    }
}
Maybe<Located<NNPtr<IR::Value>>> IR::PointerType::unary_op(Codegen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, Located<ASTNS::UnaryOperator> op, Located<NNPtr<IR::Value>> operand, ASTNS::AST const &ast) const {
    ASSERT(&operand.value->type() == this);

    ERR_UNARY_UNSUPPORTED_OP(operand, op);
    return Maybe<Located<NNPtr<Value>>>();
}
Maybe<Located<NNPtr<IR::Value>>> IR::PointerType::cast_from(Codegen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, Located<NNPtr<IR::Value>> v, ASTNS::AST const &ast) const {
    if (dynamic_cast<IR::PointerType const *>(&v.value->type())) {
        IR::Register &out = fun.add_register(*this, v.span, false);
        cur_block->add<IR::Instrs::NoOpCast>(out, v, this);
        return Located<NNPtr<IR::Value>> { ast, out };
    }

    ERR_INVALID_CAST(ast, v, *this);
    return Maybe<Located<NNPtr<Value>>>();
}
llvm::Type& IR::PointerType::to_llvm_type(llvm::LLVMContext &con) const {
    return *llvm::PointerType::getUnqual(&ty->to_llvm_type(con));
}
Located<NNPtr<IR::Value>> IR::PointerType::impl_cast(Codegen::Context &cgc, IR::Function &fun, NNPtr<IR::Block> &cur_block, Located<NNPtr<IR::Value>> v) const {
    return v;
}

