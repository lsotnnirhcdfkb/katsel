#include "codegen/codegen.h"
#include "message/errors.h"

CodeGenNS::ExprCodeGen::ExprCodeGen(CodeGen &cg): cg(cg) {}

Value CodeGenNS::ExprCodeGen::expr(ASTNS::ExprB *ast)
{
    ret = Value();
    ast->accept(this);
    return ret;
}

#define BASICBINARYOP(exprtype)                                                                                                        \
    void CodeGenNS::ExprCodeGen::visit##exprtype##Expr(ASTNS::exprtype##Expr *ast)                                                     \
    {                                                                                                                                  \
        Value lhs = expr(ast->lhs.get());                                                                                              \
        Value rhs = expr(ast->rhs.get());                                                                                              \
                                                                                                                                       \
        if (!lhs.val || !rhs.val)                                                                                                      \
        {                                                                                                                              \
            ret = Value();                                                                                                             \
            return;                                                                                                                    \
        }                                                                                                                              \
                                                                                                                                       \
        if (!lhs.type->hasOperator(ast->op.type))                                                                                      \
        {                                                                                                                              \
            Error(Error::MsgType::ERROR, ast->op, "left-hand side of binary expression does not support operator")                     \
                .underline(Error::Underline(ast->op, '^')                                                                              \
                    .error(concatMsg("type "", lhs.type->stringify(), "" does not support operator "", ast->op.stringify(), """)))     \
                .underline(Error::Underline(lhs, '~'))                                                                                 \
                .underline(Error::Underline(rhs, '-'))                                                                                 \
                .report();                                                                                                             \
            ret = Value();                                                                                                             \
            return;                                                                                                                    \
        }                                                                                                                              \
                                                                                                                                       \
        ret = lhs.type->binOp(cg.context, lhs, rhs, ast->op, ast);                                                                     \
    }

#define BASICUNARYOP(exprtype)                                                                                                                        \
    void CodeGenNS::ExprCodeGen::visit##exprtype##Expr(ASTNS::exprtype##Expr *ast)                                                                    \
    {                                                                                                                                                 \
        Value oper = expr(ast->operand.get());                                                                                                        \
        if (!oper.val)                                                                                                                                \
        {                                                                                                                                             \
            ret = Value();                                                                                                                            \
            return;                                                                                                                                   \
        }                                                                                                                                             \
                                                                                                                                                      \
        if (!oper.type->hasOperator(ast->op.type))                                                                                                    \
        {                                                                                                                                             \
            Error(Error::MsgType::ERROR, ast->operand.get(), "operand of unary expression does not support operator")                                 \
                .underline(Error::Underline(ast->op, '^')                                                                                             \
                    .error(concatMsg("operand of type \"", oper.type->stringify(), "\" does not support operator \"", ast->op.stringify(), "\"")))    \
                .underline(Error::Underline(oper, '-'))                                                                                               \
                .report();                                                                                                                            \
                ret = Value();                                                                                                                        \
                return;                                                                                                                               \
        }                                                                                                                                             \
                                                                                                                                                      \
        ret = oper.type->unaryOp(cg.context, oper, ast->op, ast);                                                                                     \
    }

BASICBINARYOP(Addition)
BASICBINARYOP(Binand)
BASICBINARYOP(Binor)
BASICBINARYOP(Bitand)
BASICBINARYOP(Bitor)
BASICBINARYOP(Bitshift)
BASICBINARYOP(Bitxor)
BASICBINARYOP(Compeq)
BASICBINARYOP(Complgt)
BASICBINARYOP(Mult)

BASICUNARYOP(Binnot)
BASICUNARYOP(Unary)

void CodeGenNS::ExprCodeGen::visitCallExpr(ASTNS::CallExpr *ast)
{
    Value func = expr(ast->callee.get());
    if (!func.val)
    {
        ret = Value();
        return;
    }

    FunctionType *fty = dynamic_cast<FunctionType*>(func.type);
    if (!fty)
    {
        Error(Error::MsgType::ERROR, ast->oparn, "value not callable")
            .underline(Error::Underline(func, '^')
                .error(concatMsg("cannot call non-function of type \"", func.type->stringify(), "\"")))
            .report();
        ret = Value();
        return;
    }

    Type *retty = fty->ret;
    std::vector<Value> args;
    if (ast->args)
        args = cg.argsVisitor.args(ast->args.get());

    if (args.size() != fty->paramtys.size())
    {
        Error(Error::MsgType::ERROR, ast->oparn, "wrong number of arguments to function call")
            .underline(Error::Underline(ast->args.get(), '^')
                .error("wrong number of arguments to function call"))
            .report();
        ret = Value();
        return;
    }

    auto i = args.begin();
    auto j = fty->paramtys.begin();
    for (; i != args.end() && j != fty->paramtys.end(); ++i, ++j)
    {
        if (i->type != *j)
        {
            Error(Error::MsgType::ERROR, *i, "wrong argument to function call")
                .underline(Error::Underline(*i, '^')
                    .error(concatMsg("wrong argument to function call: argument is of type \"", i->type->stringify(), "\""))
                    .note(concatMsg("passing to parameter of type \"", (*j)->stringify(), "\"")))
                .report();
            ret = Value();
            return;
        }
    }

    std::vector<llvm::Value*> argsasllvm;
    argsasllvm.reserve(args.size());
    for (Value const &v : args)
        argsasllvm.push_back(v.val);

    llvm::FunctionType *ftyasllvm = static_cast<llvm::FunctionType*>(fty->toLLVMType(cg.context.context));

    ret = Value(retty, cg.context.builder.CreateCall(ftyasllvm, func.val, argsasllvm), ast);
}

void CodeGenNS::ExprCodeGen::visitPrimaryExpr(ASTNS::PrimaryExpr *ast)
{
    switch (ast->value.type)
    {
        case TokenType::TRUELIT:
            ret = Value(cg.context.getBuiltinType(BuiltinType::Builtins::BOOL), cg.context.builder.getInt1(true), ast);
            return;

        case TokenType::FALSELIT:
            ret = Value(cg.context.getBuiltinType(BuiltinType::Builtins::BOOL), cg.context.builder.getInt1(false), ast);
            return;

        case TokenType::FLOATLIT:
            Error(Error::MsgType::INTERR, ast->value, "floating point literals are not supported yet")
                .underline(Error::Underline(ast->value, '^')
                    .note("coming soon!"))
                .reportAbort();
            return;

        case TokenType::NULLPTRLIT:
            Error(Error::MsgType::INTERR, ast->value, "nullptr literals are not supported yet")
                .underline(Error::Underline(ast->value, '^')
                    .error("pointers are not here yet!")
                    .note("coming soon!"))
                .reportAbort();

        case TokenType::DECINTLIT:
            ret = Value(cg.context.getBuiltinType(BuiltinType::Builtins::UINT32), cg.context.builder.getInt32(std::stoi(ast->value.stringify())), ast);
            return;

        case TokenType::OCTINTLIT:
            ret = Value(cg.context.getBuiltinType(BuiltinType::Builtins::UINT32), cg.context.builder.getInt32(std::stoi(ast->value.stringify(), nullptr, 8)), ast);
            return;

        case TokenType::BININTLIT:
            ret = Value(cg.context.getBuiltinType(BuiltinType::Builtins::UINT32), cg.context.builder.getInt32(std::stoi(ast->value.stringify(), nullptr, 2)), ast);
            return;

        case TokenType::HEXINTLIT:
            ret = Value(cg.context.getBuiltinType(BuiltinType::Builtins::UINT32), cg.context.builder.getInt32(std::stoi(ast->value.stringify(), nullptr, 16)), ast);
            return;

        case TokenType::CHARLIT:
            ret = Value(cg.context.getBuiltinType(BuiltinType::Builtins::CHAR), cg.context.builder.getInt8(*(ast->value.start + 1)), ast);
            return;

        case TokenType::STRINGLIT:
            Error(Error::MsgType::INTERR, ast->value, "string literals are not supported yet")
                .underline(Error::Underline(ast->value, '^')
                    .error("strings")
                    .note("coming soon!")
                    .note("probably after nullptr literals though!"))
                .reportAbort();

        case TokenType::IDENTIFIER:
            {
                Value v = cg.context.findValue(ast->value.stringify());
                if (!v.val)
                {
                    Error(Error::MsgType::ERROR, ast->value, "name is not defined")
                        .underline(Error::Underline(ast->value, '^')
                            .error("name is not defined"))
                        .report();
                    ret = Value();
                    return;
                }
                if (llvm::isa<llvm::AllocaInst>(v.val))
                {
                    llvm::Value *loadInst = cg.context.builder.CreateLoad(v.val);
                    ret = Value(v.type, loadInst, ast);
                }
                else
                    ret = Value(v.type, v.val, ast);
            }
            return;

        default:
            invalidTok("primary token", ast->value);
    }
}

void CodeGenNS::ExprCodeGen::visitTernaryExpr(ASTNS::TernaryExpr *ast)
{
	Value cond = expr(ast->cond.get());
    if (!cond.val)
    {
        ret = Value();
        return;
    }
    cond = cond.type->isTrue(cg.context, cond);

    llvm::Function *f = cg.context.builder.GetInsertBlock()->getParent();

    llvm::BasicBlock *trueb  = llvm::BasicBlock::Create(cg.context.context, "trueb", f);
    llvm::BasicBlock *falseb = llvm::BasicBlock::Create(cg.context.context, "falseb");
    llvm::BasicBlock *afterb = llvm::BasicBlock::Create(cg.context.context, "afterb");

    cg.context.builder.CreateCondBr(cond.val, trueb, falseb);

    cg.context.builder.SetInsertPoint(trueb);
    Value truev = expr(ast->trues.get());
    if (!truev.val)
    {
        ret = Value();
        return;
    }
    cg.context.builder.CreateBr(afterb);
    trueb = cg.context.builder.GetInsertBlock();

    f->getBasicBlockList().push_back(falseb);
    cg.context.builder.SetInsertPoint(falseb);
    Value falsev = expr(ast->falses.get());
    if (!falsev.val)
    {
        ret = Value();
        return;
    }
    cg.context.builder.CreateBr(afterb);
    falseb = cg.context.builder.GetInsertBlock();

    if (truev.type != falsev.type)
    {
        Error(Error::MsgType::ERROR, ast->colon, "Two branches of ternary conditional expression of different types")
            .underline(Error::Underline(truev, '^')
                .note(truev.type->stringify()))
            .underline(Error::Underline(falsev, '^')
                .note(falsev.type->stringify()))
            .underline(Error::Underline(ast->colon, '-'))
            .underline(Error::Underline(ast->quest, '-'))
            .report();
        ret = Value();
        return;
    }

    f->getBasicBlockList().push_back(afterb);
    cg.context.builder.SetInsertPoint(afterb);

    llvm::PHINode *phi = cg.context.builder.CreatePHI(truev.type->toLLVMType(cg.context.context), 2);
    phi->addIncoming(truev.val, trueb);
    phi->addIncoming(falsev.val, falseb);
    ret = Value(truev.type, phi, ast);
}

void CodeGenNS::ExprCodeGen::visitAssignmentExpr(ASTNS::AssignmentExpr *ast)
{
    Value lhs = expr(ast->target.get());
    Value rhs = expr(ast->value.get());

    if (!lhs.val|| !rhs.val)
    {
        ret = Value();
        return;
    }

    if (!llvm::isa<llvm::LoadInst>(lhs.val))
    {
        Error(Error::MsgType::ERROR, ast->equal, "Invalid assignment target")
            .underline(Error::Underline(ast->equal, '^')
                .error("Invalid assignment target"))
            .underline(Error::Underline(lhs, '~'))
            .report();
        ret = Value();
        return;
    }
    llvm::LoadInst *load = static_cast<llvm::LoadInst*>(lhs.val);

    Value target = Value(lhs.type, load->getPointerOperand(), ast->target.get());

    if (target.type != rhs.type)
    {
        Error(Error::MsgType::ERROR, ast->equal, "Assignment target and value do not have same type")
            .underline(Error::Underline(rhs, '^')
                .note(rhs.type->stringify()))
            .underline(Error::Underline(target, '^')
                .note(target.type->stringify()))
            .underline(Error::Underline(ast->equal, '-'))
            .report();
        ret = Value();
        return;
    }
    cg.context.builder.CreateStore(rhs.val, target.val);
    load->eraseFromParent(); // dont need the load anymore

    ret = rhs;
    return;
}

void CodeGenNS::ExprCodeGen::visitExpr(ASTNS::Expr *) {}
