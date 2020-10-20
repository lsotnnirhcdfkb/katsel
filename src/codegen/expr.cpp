#include "codegen/codegen.h"
#include "message/fmtmessage.h"
#include "message/errors.h"

#include <iostream>

void CodeGen::visitBinaryExpr(ASTNS::BinaryExpr *a)
{
    // TODO: assignment
    Value lhs = evalExpr(a->lhs.get());
    Value rhs = evalExpr(a->rhs.get());

    if (!lhs.val || !rhs.val)
        return;

    if (a->op.type == TokenType::EQUAL)
    {
        if (!llvm::isa<llvm::LoadInst>(lhs.val))
        {
            report(MsgType::ERROR, msg::invalidAssign(), a, a->lhs.get(), a->rhs.get(), a->op);
            return;
        }
        llvm::LoadInst *load = static_cast<llvm::LoadInst*>(lhs.val);

        Value target = Value(lhs.type, load->getPointerOperand()); // TODO: cast type 
        Value assignment = rhs.type->castTo(context, rhs, target.type);

        context.builder.CreateStore(assignment.val, target.val);
        load->eraseFromParent(); // dont need the load anymore

        exprRetVal = assignment;
        return;
    }

    if (!lhs.type->hasOperator(a->op.type))
    {
        report(MsgType::ERROR, msg::typeNoOp(lhs.type, a->op), a, a->op, a->lhs.get());
        return;
    }

    exprRetVal = lhs.type->binOp(context, lhs, rhs, a->op);
}

void CodeGen::visitUnaryExpr(ASTNS::UnaryExpr *a)
{
	Value oper = evalExpr(a->operand.get());
    if (!oper.val)
        return;

    if (!oper.type->hasOperator(a->op.type))
    {
        report(MsgType::ERROR, msg::typeNoOp(oper.type, a->op), a, a->op, a->operand.get());
        exprRetVal = Value();
        return;
    }

    exprRetVal = oper.type->unaryOp(context, oper, a->op);
}

void CodeGen::visitTernaryExpr(ASTNS::TernaryExpr *a)
{
	Value cond = evalExpr(a->condition.get());
    if (!cond.val) return;
    cond = cond.type->isTrue(context, cond);

    llvm::Function *f = context.builder.GetInsertBlock()->getParent();
    llvm::BasicBlock *trueb  = llvm::BasicBlock::Create(context.context, "trueb", f);
    llvm::BasicBlock *falseb = llvm::BasicBlock::Create(context.context, "falseb");
    llvm::BasicBlock *afterb = llvm::BasicBlock::Create(context.context, "afterb");

    context.builder.CreateCondBr(cond.val, trueb, falseb);

    context.builder.SetInsertPoint(trueb);
    Value truev = evalExpr(a->trues.get());
    if (!truev.val) return;
    trueb = context.builder.GetInsertBlock();
    context.builder.CreateBr(afterb);

    f->getBasicBlockList().push_back(falseb);
    context.builder.SetInsertPoint(falseb);
    Value falsev = evalExpr(a->falses.get());
    if (!falsev.val) return;
    falseb = context.builder.GetInsertBlock();
    context.builder.CreateBr(afterb);

    f->getBasicBlockList().push_back(afterb);
    context.builder.SetInsertPoint(afterb);

    truev.type->castTwoVals(context, truev, falsev);
    llvm::PHINode *phi = context.builder.CreatePHI(truev.type->toLLVMType(context.context), 2);
    phi->addIncoming(truev.val, trueb);
    phi->addIncoming(falsev.val, falseb);
    exprRetVal = Value(truev.type, phi);
}

void CodeGen::visitPrimaryExpr(ASTNS::PrimaryExpr *a)
{
    Value ret;
    switch (a->value.type)
    {
        case TokenType::TRUELIT:
            ret = Value(context.getBuiltinType(BuiltinType::Builtins::BOOL), context.builder.getInt1(true));
            break;

        case TokenType::FALSELIT:
            ret = Value(context.getBuiltinType(BuiltinType::Builtins::BOOL), context.builder.getInt1(false));
            break;

        case TokenType::FLOATLIT:
            break;

        case TokenType::NULLPTRLIT:
            report(MsgType::INTERNALERR, "NULLPTR literal is not supported because we do not have pointers yet", a, a);
            break;

        case TokenType::DECINTLIT:
            ret = Value(context.getBuiltinType(BuiltinType::Builtins::UINT32), context.builder.getInt32(std::stoi(tokenToStr(a->value))));
            break;

        case TokenType::OCTINTLIT:
            ret = Value(context.getBuiltinType(BuiltinType::Builtins::UINT32), context.builder.getInt32(std::stoi(tokenToStr(a->value), nullptr, 8)));
            break;

        case TokenType::BININTLIT:
            ret = Value(context.getBuiltinType(BuiltinType::Builtins::UINT32), context.builder.getInt32(std::stoi(tokenToStr(a->value), nullptr, 2)));
            break;

        case TokenType::HEXINTLIT:
            ret = Value(context.getBuiltinType(BuiltinType::Builtins::UINT32), context.builder.getInt32(std::stoi(tokenToStr(a->value), nullptr, 16)));
            break;

        case TokenType::CHARLIT:
            ret = Value(context.getBuiltinType(BuiltinType::Builtins::CHAR), context.builder.getInt8(*(a->value.start + 1)));
            break;

        case TokenType::STRINGLIT:
            std::cerr << "string literals are not supported yet" << std::endl;
            break;

        case TokenType::IDENTIFIER:
            {
                Value v = context.findValue(tokenToStr(a->value));
                if (!v.val)
                {
                    report(MsgType::ERROR, msg::undefVar(), a->value, a->value);
                    return;
                }
                if (llvm::isa<llvm::AllocaInst>(v.val))
                {
                    llvm::Value *loadInst = context.builder.CreateLoad(v.val);
                    ret = Value(v.type, loadInst);
                }
                else
                    ret = v;
            }
            break;

        default:
            report(MsgType::INTERNALERR, "Invalid primary token", a->value, a->value);
    }
    exprRetVal = ret;
}

void CodeGen::visitCallExpr(ASTNS::CallExpr *a)
{
    Value func = evalExpr(a->func.get());
    if (!func.val)
        return;

    FunctionType *fty = dynamic_cast<FunctionType*>(func.type);
    if (!fty)
    {
        report(MsgType::ERROR, msg::cannotCall(), a, a->func.get());
        return;
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
        std::cerr << "Wrong number of arguments to function" << std::endl;
        return;
    }

    auto i = args.begin();
    auto j = fty->paramtys.begin();
    for (; i != args.end() && j != fty->paramtys.end(); ++i, ++j)
    {
        if (i->type != *j)
        {
            std::cerr << "Incorrect argument to function call" << std::endl;
            return;
        }
    }

    llvm::FunctionType *ftyasllvm = static_cast<llvm::FunctionType*>(fty->toLLVMType(context.context));

    exprRetVal = Value(ret, context.builder.CreateCall(ftyasllvm, func.val, argsasllvm));
}

