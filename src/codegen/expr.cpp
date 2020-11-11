#include "codegen/codegen.h"
#include "message/errors.h"

#include <iostream>
#include <sstream>

void CodeGen::visitBinaryExpr(ASTNS::BinaryExpr *a)
{
    Value lhs = evalExpr(a->lhs.get());
    Value rhs = evalExpr(a->rhs.get());

    if (!lhs.val || !rhs.val)
        CG_RETURNNULL();

    if (a->op.type == TokenType::EQUAL)
    {
        if (!llvm::isa<llvm::LoadInst>(lhs.val))
        {
            Error(Error::MsgType::ERROR, a->op, "Invalid assignment target")
                .primary(Error::Primary(a->op)
                    .error("Invalid assignment target"))
                .secondary(a->lhs.get())
                .report();
            CG_RETURNNULL();
        }
        llvm::LoadInst *load = static_cast<llvm::LoadInst*>(lhs.val);

        Value target = Value(lhs.type, load->getPointerOperand(), a->lhs.get());
        
        if (target.type != rhs.type)
        {
            Error(Error::MsgType::ERROR, a->op, "Assignment target and value do not have same type")
                .primary(Error::Primary(rhs)
                    .note(rhs.type->stringify()))
                .primary(Error::Primary(target)
                    .note(target.type->stringify()))
                .secondary(a->op)
                .report();
            CG_RETURNNULL();
        }
        context.builder.CreateStore(rhs.val, target.val);
        load->eraseFromParent(); // dont need the load anymore

        exprRetVal = rhs;
        return;
    }

    if (!lhs.type->hasOperator(a->op.type))
    {
        Error(Error::MsgType::ERROR, a->op, "Left-hand side of binary expression does not support operator")
            .primary(Error::Primary(a->op)
                .error(static_cast<std::stringstream>(std::stringstream() << "Type \"" << lhs.type->stringify() << "\" does not support operator \"" << a->op.stringify() << "\"").str()))
            .secondary(lhs)
            .report();
        CG_RETURNNULL();
    }

    exprRetVal = lhs.type->binOp(context, lhs, rhs, a->op, a);
}

void CodeGen::visitUnaryExpr(ASTNS::UnaryExpr *a)
{
	Value oper = evalExpr(a->operand.get());
    if (!oper.val)
        CG_RETURNNULL();

    if (!oper.type->hasOperator(a->op.type))
    {
        Error(Error::MsgType::ERROR, a->operand.get(), "Operand of unary expression does not support operator")
            .primary(Error::Primary(a->op)
                .error(static_cast<std::stringstream>(std::stringstream() << "Type \"" << oper.type->stringify() << "\" does not support operator \"" << a->op.stringify() << "\"").str()))
            .secondary(oper)
            .report();
        CG_RETURNNULL();
    }

    exprRetVal = oper.type->unaryOp(context, oper, a->op, a);
}

void CodeGen::visitTernaryExpr(ASTNS::TernaryExpr *a)
{
	Value cond = evalExpr(a->condition.get());
    if (!cond.val) CG_RETURNNULL();
    cond = cond.type->isTrue(context, cond);

    llvm::Function *f = context.builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *trueb  = llvm::BasicBlock::Create(context.context, "trueb", f);
    llvm::BasicBlock *falseb = llvm::BasicBlock::Create(context.context, "falseb");
    llvm::BasicBlock *afterb = llvm::BasicBlock::Create(context.context, "afterb");

    context.builder.CreateCondBr(cond.val, trueb, falseb);

    context.builder.SetInsertPoint(trueb);
    Value truev = evalExpr(a->trues.get());
    if (!truev.val) CG_RETURNNULL();
    context.builder.CreateBr(afterb);
    trueb = context.builder.GetInsertBlock();

    f->getBasicBlockList().push_back(falseb);
    context.builder.SetInsertPoint(falseb);
    Value falsev = evalExpr(a->falses.get());
    if (!falsev.val) CG_RETURNNULL();
    context.builder.CreateBr(afterb);
    falseb = context.builder.GetInsertBlock();

    if (truev.type != falsev.type)
    {
        Error(Error::MsgType::ERROR, a->condition.get(), "Two branches of ternary conditional expression of different types")
            .primary(Error::Primary(truev)
                .note(truev.type->stringify()))
            .primary(Error::Primary(falsev)
                .note(falsev.type->stringify()))
            .secondary(a->condition.get())
            .report();
        CG_RETURNNULL();
    }

    f->getBasicBlockList().push_back(afterb);
    context.builder.SetInsertPoint(afterb);

    llvm::PHINode *phi = context.builder.CreatePHI(truev.type->toLLVMType(context.context), 2);
    phi->addIncoming(truev.val, trueb);
    phi->addIncoming(falsev.val, falseb);
    exprRetVal = Value(truev.type, phi, a);
}

void CodeGen::visitPrimaryExpr(ASTNS::PrimaryExpr *a)
{
    Value ret;
    switch (a->value.type)
    {
        case TokenType::TRUELIT:
            ret = Value(context.getBuiltinType(BuiltinType::Builtins::BOOL), context.builder.getInt1(true), a);
            break;

        case TokenType::FALSELIT:
            ret = Value(context.getBuiltinType(BuiltinType::Builtins::BOOL), context.builder.getInt1(false), a);
            break;

        case TokenType::FLOATLIT:
            break;

        case TokenType::NULLPTRLIT:
            Error(Error::MsgType::INTERR, a->value, "nullptr literals are not supported yet")
                .primary(Error::Primary(a->value)
                    .error("pointers are not here yet!")
                    .note("coming soon!"))
                .reportAbort();

        case TokenType::DECINTLIT:
            ret = Value(context.getBuiltinType(BuiltinType::Builtins::UINT32), context.builder.getInt32(std::stoi(a->value.stringify())), a);
            break;

        case TokenType::OCTINTLIT:
            ret = Value(context.getBuiltinType(BuiltinType::Builtins::UINT32), context.builder.getInt32(std::stoi(a->value.stringify(), nullptr, 8)), a);
            break;

        case TokenType::BININTLIT:
            ret = Value(context.getBuiltinType(BuiltinType::Builtins::UINT32), context.builder.getInt32(std::stoi(a->value.stringify(), nullptr, 2)), a);
            break;

        case TokenType::HEXINTLIT:
            ret = Value(context.getBuiltinType(BuiltinType::Builtins::UINT32), context.builder.getInt32(std::stoi(a->value.stringify(), nullptr, 16)), a);
            break;

        case TokenType::CHARLIT:
            ret = Value(context.getBuiltinType(BuiltinType::Builtins::CHAR), context.builder.getInt8(*(a->value.start + 1)), a);
            break;

        case TokenType::STRINGLIT:
            Error(Error::MsgType::INTERR, a->value, "string literals are not supported yet")
                .primary(Error::Primary(a->value)
                    .error("strings")
                    .note("coming soon!")
                    .note("probably after nullptr literals though!"))
                .reportAbort();

        case TokenType::IDENTIFIER:
            {
                Value v = context.findValue(a->value.stringify());
                if (!v.val)
                {
                    Error(Error::MsgType::ERROR, a->value, "Name is not defined")
                        .primary(Error::Primary(a->value)
                            .error("Name is not defined"))
                        .report();
                    CG_RETURNNULL();
                }
                if (llvm::isa<llvm::AllocaInst>(v.val))
                {
                    llvm::Value *loadInst = context.builder.CreateLoad(v.val);
                    ret = Value(v.type, loadInst, a);
                }
                else
                    ret = Value(v.type, v.val, a);
            }
            break;

        default:
            invalidTok("primary token", a->value);
    }
    exprRetVal = ret;
}

void CodeGen::visitCallExpr(ASTNS::CallExpr *a)
{
    Value func = evalExpr(a->func.get());
    if (!func.val)
        CG_RETURNNULL();

    FunctionType *fty = dynamic_cast<FunctionType*>(func.type);
    if (!fty)
    {
        Error(Error::MsgType::ERROR, func, "Value not callable")
            .primary(Error::Primary(func)
                .error("Cannot call non-function"))
            .report();
        CG_RETURNNULL();
    }

    Type *ret = fty->ret;
    std::vector<Value> args;
    std::vector<llvm::Value*> argsasllvm;

    {
        ASTNS::Arg *arg = a->args.get();
        while (arg)
        {
            Value varg = evalExpr(arg->value.get());
            if (!varg.val)
                continue;
            args.push_back(varg);
            argsasllvm.push_back(args.back().val);
            arg = arg->next.get();
        }
    }

    if (args.size() != fty->paramtys.size())
    {
        Error(Error::MsgType::ERROR, a, "Wrong number of arguments to function call")
            .primary(Error::Primary(a)
                .error("Wrong number of arguments to function call"))
            .report();
        CG_RETURNNULL();
    }

    auto i = args.begin();
    auto j = fty->paramtys.begin();
    for (; i != args.end() && j != fty->paramtys.end(); ++i, ++j)
    {
        if (i->type != *j)
        {
            Error(Error::MsgType::ERROR, *i, "Wrong argument to function call")
                .primary(Error::Primary(*i)
                    .error("Wrong argumnet to function call")
                    .note(static_cast<std::stringstream>(std::stringstream() << "Argument is of type \"" << i->type->stringify() << "\", but is being passed to parameter of type \"" << (*j)->stringify() << "\"").str()))
                .report();
            CG_RETURNNULL();
        }
    }

    llvm::FunctionType *ftyasllvm = static_cast<llvm::FunctionType*>(fty->toLLVMType(context.context));

    exprRetVal = Value(ret, context.builder.CreateCall(ftyasllvm, func.val, argsasllvm), a);
}

