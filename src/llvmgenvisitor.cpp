#include "llvmgenvisitor.h"

LLVMGenVisitor::LLVMGenVisitor(File &sourcefile): sourcefile(sourcefile), builder(context), module_(std::make_unique<llvm::Module>("COxianc output of file " + sourcefile.filename, context)), scopenum(0) {}

// {{{ visiting asts
void LLVMGenVisitor::visitProgramAST(const ProgramAST *ast) 
{
    for (const std::unique_ptr<AST> &dast : ast->asts) 
    {
        dast->accept(this);
    }

    module_->print(llvm::outs(), nullptr);
}
// {{{ declaration visiting
void LLVMGenVisitor::visitFunctionAST(const FunctionAST *ast)
{
    std::string name = std::string(ast->name.start, ast->name.end);
    llvm::FunctionType *ft = llvm::FunctionType::get(llvm::Type::getVoidTy(context), false); 
    llvm::Function *f = llvm::Function::Create(ft, llvm::Function::ExternalLinkage, name, *module_);

    beginNewScope();



    llvm::BasicBlock *block = llvm::BasicBlock::Create(context, name + "entry", f);
    builder.SetInsertPoint(block);

    ast->body->accept(this);

    builder.CreateRetVoid();
    llvm::verifyFunction(*f); 

    finishCurScope();
    curRetVal = f;
}
// }}}
// {{{ expression visiting
// {{{ binary ast
void LLVMGenVisitor::visitBinaryAST(const BinaryAST *ast) 
{
    ast->last->accept(this);
    llvm::Value *lval = curRetVal;
    ast->rast->accept(this);
    llvm::Value *rval = curRetVal;

    if (!lval || !rval) 
    {
        curRetVal = nullptr;
        return;
    }

    llvm::Value *retval = nullptr;

    switch (ast->op.type)
    {
        // TODO: change instructions to depend on the lval and rval types
        case TokenType::DOUBLEPIPE:
            retval = builder.CreateOr(lval, rval);
            break;

        case TokenType::DOUBLEAMPER:
            retval = builder.CreateAnd(lval, rval);
            break;

        case TokenType::BANGEQUAL:
            retval = builder.CreateICmpNE(lval, rval);
            break;

        case TokenType::DOUBLEEQUAL:
            retval = builder.CreateICmpEQ(lval, rval);
            break;

        case TokenType::LESS:
            retval = builder.CreateICmpULT(lval, rval);
            break;

        case TokenType::GREATER:
            retval = builder.CreateICmpUGT(lval, rval);
            break;

        case TokenType::LESSEQUAL:
            retval = builder.CreateICmpULE(lval, rval);
            break;

        case TokenType::GREATEREQUAL:
            retval = builder.CreateICmpUGE(lval, rval);
            break;

        case TokenType::CARET:
            retval = builder.CreateXor(lval, rval);
            break;

        case TokenType::PIPE:
            retval = builder.CreateOr(lval, rval);
            break;

        case TokenType::AMPER:
            retval = builder.CreateAnd(lval, rval);
            break;

        case TokenType::DOUBLELESS:
            retval = builder.CreateShl(lval, rval);
            break;

        case TokenType::DOUBLEGREATER:
            retval = builder.CreateLShr(lval, rval);
            break;

        case TokenType::PLUS:
            retval = builder.CreateAdd(lval, rval);
            break;

        case TokenType::MINUS:
            retval = builder.CreateSub(lval, rval);
            break;

        case TokenType::STAR:
            retval = builder.CreateMul(lval, rval);
            break;

        case TokenType::SLASH:
            retval = builder.CreateUDiv(lval, rval);
            break;

        case TokenType::PERCENT:
            retval = builder.CreateURem(lval, rval);
            break;

        default: reportError(ast->op, "invalid thingy", sourcefile); retval = nullptr;
    }

    curRetVal = retval;
}
// }}}
// {{{ ternary ast
void LLVMGenVisitor::visitTernaryOpAST(const TernaryOpAST *ast) 
{
    ast->conditional->accept(this);
    llvm::Value *cond = curRetVal;

    cond = builder.CreateICmpNE(cond, llvm::ConstantInt::get(context, llvm::APInt(64, 0)));

    llvm::Function *func = builder.GetInsertBlock()->getParent();

    auto *trueb = llvm::BasicBlock::Create(context, "trueblock", func);
    auto *falseb = llvm::BasicBlock::Create(context, "falseblock");
    auto *afterb = llvm::BasicBlock::Create(context, "afterblock");

    builder.CreateCondBr(cond, trueb, falseb);
    builder.SetInsertPoint(trueb);

    ast->trueast->accept(this);
    llvm::Value *truev = curRetVal;

    builder.CreateBr(afterb);
    trueb = builder.GetInsertBlock();

    func->getBasicBlockList().push_back(falseb);
    builder.SetInsertPoint(falseb);

    ast->falseast->accept(this);
    llvm::Value *falsev = curRetVal;

    builder.CreateBr(afterb);
    falseb = builder.GetInsertBlock();

    func->getBasicBlockList().push_back(afterb);
    builder.SetInsertPoint(afterb);
    llvm::PHINode *phi = builder.CreatePHI(llvm::Type::getInt64Ty(context), 2);

    phi->addIncoming(truev, trueb);
    phi->addIncoming(falsev, falseb);

    curRetVal = phi;
}
// }}}
// {{{ unary ast
void LLVMGenVisitor::visitUnaryAST(const UnaryAST *ast) 
{
    ast->ast->accept(this);
    llvm::Value *val = curRetVal;

    if (val)
    {
        curRetVal = nullptr;
        return;
    }

    llvm::Value *retval = nullptr;

    switch (ast->op.type)
    {
        // TODO: change instructions to depend on the lval and rval types
        case TokenType::BANG:
            retval = builder.CreateNot(val);
            break;

        case TokenType::TILDE:
            // return x ^ 2^64-1
            retval = builder.CreateXor(val, llvm::ConstantInt::get(context, llvm::APInt(64, 0xffffffffffffffff)));
            break;

        case TokenType::MINUS:
            // return 0-x
            retval = builder.CreateSub(llvm::ConstantInt::get(context, llvm::APInt(64, 0)), val);
            break;

        default: reportError(ast->op, "invalid thingy", sourcefile); retval = nullptr;
    }

    curRetVal = retval;
}
// }}}
// {{{ assign ast
void LLVMGenVisitor::visitAssignAST(const AssignAST *ast)
{
    VariableRefAST *lhs = dynamic_cast<VariableRefAST*>(ast->lhs.get());

    if (!lhs)
    {
        reportError(ast->equalSign, "Invalid target for assignment", sourcefile);
        curRetVal = nullptr;
        return;
    }

    ast->rhs->accept(this);
    llvm::Value *rhs = curRetVal;

    std::string name = std::string(lhs->var.start, lhs->var.end);
    llvm::Value *var = getVarFromName(name, ast->equalSign);
    if (!var)
    {
        curRetVal = nullptr;
        return;
    }

    builder.CreateStore(rhs, var);
    curRetVal = rhs;
}
// }}}
// {{{ var ref
void LLVMGenVisitor::visitVariableRefAST(const VariableRefAST *ast)
{
    std::string name = std::string(ast->var.start, ast->var.end);
    llvm::Value *v = getVarFromName(name, ast->var);

    if (v)
    {
        curRetVal = builder.CreateLoad(v, name.c_str());
        return;
    }

    curRetVal = nullptr;
}
// }}}
// {{{ statement visiting
void LLVMGenVisitor::visitExprStmtAST(const ExprStmtAST *ast) 
{
    ast->ast->accept(this);
    curRetVal = nullptr;
}

void LLVMGenVisitor::visitVarStmtAST(const VarStmtAST *ast) 
{
    std::string varname = std::string(ast->name.start, ast->name.end);
    llvm::Function *f = builder.GetInsertBlock()->getParent();
    llvm::AllocaInst *varalloca = createEntryAlloca(f, varname);

    ast->expression->accept(this);
    llvm::Value *value = curRetVal;

    builder.CreateStore(value, varalloca);

    scopesymbols[std::pair<int, std::string>{scopenum, varname}] = varalloca;
    
    curRetVal = varalloca;
}
// }}}
// {{{ helper ast visiting
void LLVMGenVisitor::visitTypeAST(const TypeAST *ast) 
{
    // curRetVal = llvm::Type::getInt64Ty(context);
}

void LLVMGenVisitor::visitBlockAST(const BlockAST *ast) 
{
    for (const std::unique_ptr<AST> &bast : ast->stmts) 
    {
        bast->accept(this);
    }
    curRetVal = nullptr;
}

void LLVMGenVisitor::visitArgAST(const ArgAST *ast) 
{

}

void LLVMGenVisitor::visitArgsAST(const ArgsAST *ast) 
{

}
// }}}
void LLVMGenVisitor::visitPrimaryAST(const PrimaryAST *ast) 
{
    curRetVal = llvm::ConstantInt::get(context, llvm::APInt(64, std::stoi(std::string(ast->value.start, ast->value.end))));
}
// }}}
// }}}
// {{{ private llvm visitor helper methods
llvm::AllocaInst* LLVMGenVisitor::createEntryAlloca(llvm::Function *f, const std::string &name) 
{
    llvm::IRBuilder<> b (&(f->getEntryBlock()), f->getEntryBlock().begin());
    return b.CreateAlloca(llvm::Type::getInt64Ty(context), 0, name.c_str());
}

void LLVMGenVisitor::beginNewScope()
{
    ++scopenum;
}

void LLVMGenVisitor::finishCurScope()
{
    for (auto it = scopesymbols.crbegin(); it != scopesymbols.crend(); ++it)
    {
        if (it->first.first == scopenum)
        {
            scopesymbols.erase(--it.base());
        }
    }
    --scopenum;
}

llvm::Value* LLVMGenVisitor::getVarFromName(std::string &name, Token const &tok)
{
    int highestScope = -1;
    llvm::Value *v = nullptr;
    for (auto it = scopesymbols.cbegin(); it != scopesymbols.cend(); ++it)
    {
        if (it->first.second == name && it->first.first > highestScope)
        {
            v = it->second;
            highestScope = it->first.first;
        }
    }

    if (!v)
        reportError(tok, "unknown variable name", sourcefile);

    return v; // return nullptr if error
}
// }}}

// {{{ helper visitors
namespace LLVMGenVisitorHelpersNS
{
    
}
// }}}
