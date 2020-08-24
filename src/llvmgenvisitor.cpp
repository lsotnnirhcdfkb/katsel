#include "llvmgenvisitor.h"

#define LLVMGENVISITOR_RETURN(x) curRetVal = x; \
                                             return;
#define CLEARRET curRetVal = nullptr

LLVMGenVisitor::LLVMGenVisitor(File &sourcefile): sourcefile(sourcefile), builder(context), module_(std::make_unique<llvm::Module>("COxianc output of file " + sourcefile.filename, context)), scopenum(0), argsVisitor(sourcefile, context), typeVisitor(sourcefile, context) {}

// {{{ visiting asts
void LLVMGenVisitor::visitProgramAST(const ProgramAST *ast) 
{
    CLEARRET;
    for (const std::unique_ptr<AST> &dast : ast->asts) 
    {
        dast->accept(this);
    }

    module_->print(llvm::outs(), nullptr);
}
// {{{ declaration visiting
void LLVMGenVisitor::visitFunctionAST(const FunctionAST *ast)
{
    CLEARRET;
    std::vector<llvm::Type*> argTypes;
    std::vector<Token> argNames;

    if (ast->args)
    {
        ast->args->accept(&argsVisitor);

        argTypes = argsVisitor.argTypes;
        argNames = argsVisitor.argNames;
    }

    ast->type->accept(&typeVisitor);
    llvm::Type *rettype = typeVisitor.rettype;
    std::string name = std::string(ast->name.start, ast->name.end);
    llvm::FunctionType *ft = llvm::FunctionType::get(rettype, argTypes, false); 
    llvm::Function *f = llvm::Function::Create(ft, llvm::Function::ExternalLinkage, name, *module_);

    llvm::BasicBlock *block = llvm::BasicBlock::Create(context, name + "entry", f);
    builder.SetInsertPoint(block);

    beginNewScope();

    { 
        int i = 0;
        for (auto &arg : f->args())
        {
            std::string argName = std::string(argNames[i].start, argNames[i].end);
            arg.setName(argName);

            llvm::AllocaInst *alloca = createEntryAlloca(f, argName);
            builder.CreateStore(&arg, alloca);
            createScopeSymbol(argName, alloca);

            ++i;
        }
    }

    ast->body->accept(this);

    llvm::verifyFunction(*f); 

    finishCurScope();
    LLVMGENVISITOR_RETURN(f);
}
// }}}
// {{{ expression visiting
// {{{ binary ast
void LLVMGenVisitor::visitBinaryAST(const BinaryAST *ast) 
{
    CLEARRET;
    ast->last->accept(this);
    llvm::Value *lval = curRetVal;
    ast->rast->accept(this);
    llvm::Value *rval = curRetVal;

    if (!lval || !rval) 
    {
        LLVMGENVISITOR_RETURN(nullptr);
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

        default: reportError(ast->op, "invalid thingy", sourcefile); retval = nullptr; // shouldn't ever get here
    }

    LLVMGENVISITOR_RETURN(retval);
}
// }}}
// {{{ ternary ast
void LLVMGenVisitor::visitTernaryOpAST(const TernaryOpAST *ast) 
{
    CLEARRET;
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

    LLVMGENVISITOR_RETURN(phi)
}
// }}}
// {{{ unary ast
void LLVMGenVisitor::visitUnaryAST(const UnaryAST *ast) 
{
    CLEARRET;
    ast->ast->accept(this);
    llvm::Value *val = curRetVal;

    if (val)
    {
        LLVMGENVISITOR_RETURN(nullptr)
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

        default: reportError(ast->op, "invalid thingy", sourcefile); retval = nullptr; // shouldn't ever get here
    }

    LLVMGENVISITOR_RETURN(retval)
}
// }}}
// {{{ assign ast
void LLVMGenVisitor::visitAssignAST(const AssignAST *ast)
{
    CLEARRET;
    VariableRefAST *lhs = dynamic_cast<VariableRefAST*>(ast->lhs.get());

    if (!lhs)
    {
        reportError(ast->equalSign, "Invalid target for assignment", sourcefile);
        LLVMGENVISITOR_RETURN(nullptr)
    }

    ast->rhs->accept(this);
    llvm::Value *rhs = curRetVal;

    std::string name = std::string(lhs->var.start, lhs->var.end);
    llvm::Value *var = getVarFromName(name, ast->equalSign);
    if (!var)
    {
        LLVMGENVISITOR_RETURN(nullptr)
    }

    builder.CreateStore(rhs, var);
    LLVMGENVISITOR_RETURN(rhs)
}
// }}}
// {{{ var ref
void LLVMGenVisitor::visitVariableRefAST(const VariableRefAST *ast)
{
    CLEARRET;
    std::string name = std::string(ast->var.start, ast->var.end);
    llvm::Value *v = getVarFromName(name, ast->var);

    if (v)
    {
        LLVMGENVISITOR_RETURN(builder.CreateLoad(v, name.c_str()))
    }

    LLVMGENVISITOR_RETURN(nullptr)
}
// }}}
void LLVMGenVisitor::visitPrimaryAST(const PrimaryAST *ast) 
{
    CLEARRET;
    LLVMGENVISITOR_RETURN(llvm::ConstantInt::get(context, llvm::APInt(64, std::stoi(std::string(ast->value.start, ast->value.end)))))
}
// }}}
// {{{ statement visiting
void LLVMGenVisitor::visitExprStmtAST(const ExprStmtAST *ast) 
{
    CLEARRET;
    ast->ast->accept(this);
    LLVMGENVISITOR_RETURN(nullptr)
}

void LLVMGenVisitor::visitVarStmtAST(const VarStmtAST *ast) 
{
    CLEARRET;
    // TODO: types
    std::string varname = std::string(ast->name.start, ast->name.end);

    // find variable with error override
    if (getVarFromName(varname, ast->name, true))
    {
        reportError(ast->name, "cannot redefine variable", sourcefile);
        LLVMGENVISITOR_RETURN(nullptr);
    }

    llvm::Function *f = builder.GetInsertBlock()->getParent();
    llvm::AllocaInst *varalloca = createEntryAlloca(f, varname);

    ast->expression->accept(this);
    llvm::Value *value = curRetVal;

    if (!value)
    {
        LLVMGENVISITOR_RETURN(nullptr)
    }

    builder.CreateStore(value, varalloca);

    createScopeSymbol(varname, varalloca);
    
    LLVMGENVISITOR_RETURN(varalloca)
}
void LLVMGenVisitor::visitReturnStmtAST(const ReturnStmtAST *ast)
{
    CLEARRET;
    ast->expr->accept(this);
    builder.CreateRet(curRetVal);
}
// }}}
// {{{ helper ast visiting
void LLVMGenVisitor::visitTypeAST(const TypeAST *ast) 
{
    CLEARRET;
    // LLVMGENVISITOR_RETURN(llvm::Type::getInt64Ty(context))
}

void LLVMGenVisitor::visitBlockAST(const BlockAST *ast) 
{
    CLEARRET;
    beginNewScope();
    for (const std::unique_ptr<AST> &bast : ast->stmts) 
    {
        bast->accept(this);
    }
    finishCurScope();
    LLVMGENVISITOR_RETURN(nullptr)
}

void LLVMGenVisitor::visitArgAST(const ArgAST *ast) 
{
    CLEARRET;
    // shouldn't ever happen beacause ArgsVisitor processes the args
    // instead of LLVMGenVisitor
}

void LLVMGenVisitor::visitArgsAST(const ArgsAST *ast) 
{
    CLEARRET;
    // also shouldn't ever happen beacause ArgsVisitor processes the args
    // instead of LLVMGenVisitor
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
    // mostly stolen from https://stackoverflow.com/questions/7007802/erase-specific-elements-in-stdmap
    for (auto it = scopesymbols.cbegin(); it != scopesymbols.cend(); )
    {
        if (it->first.first >= scopenum) // shouldnt ever be greater than but just to make sure
        {
            scopesymbols.erase(it++);
        }
        else
        {
            ++it;
        }
    }

    --scopenum;
}

llvm::Value* LLVMGenVisitor::getVarFromName(std::string &name, Token const &tok, bool overrideErr)
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

    if (!v && !overrideErr)
        reportError(tok, "unknown variable name", sourcefile);

    return v; // return nullptr if error
}
void LLVMGenVisitor::createScopeSymbol(std::string &name, llvm::AllocaInst* alloca)
{
    scopesymbols[std::pair<int, std::string>{scopenum, name}] = alloca;
}
// }}}
// {{{ helper visitors
namespace LLVMGenVisitorHelpersNS
{
    ArgsVisitor::ArgsVisitor(File &sourcefile, llvm::LLVMContext &context): sourcefile(sourcefile), context(context) {}
    TypeVisitor::TypeVisitor(File &sourcefile, llvm::LLVMContext &context): sourcefile(sourcefile), context(context) {}

    void ArgsVisitor::visitArgAST(const ArgAST *ast)
    {
        // if this is part of a visitArgs then this will be overrided anyway
        // but if it is not then the return value is provided in a vector like it's supposed to be
        argTypes = {llvm::Type::getInt64Ty(context)}; 
        argNames = {ast->argname};
    }
    void ArgsVisitor::visitArgsAST(const ArgsAST *ast)
    {
        std::vector<llvm::Type*> cargTypes;
        std::vector<Token> cargNames;

        for (std::unique_ptr<AST> const &arg : ast->args)
        {
            arg->accept(this);
            // retval is length 1 because visitArgsAST always does that
            cargTypes.push_back(argTypes[0]);
            cargNames.push_back(argNames[0]);
        }

        argTypes = cargTypes;
        argNames = cargNames;
    }

    void TypeVisitor::visitTypeAST(const TypeAST *ast)
    {
        // right now only int64s are supported
        // TODO: types
        rettype = llvm::Type::getInt64Ty(context);
        // rettype = llvm::Type::getVoidTy(context);
    }

}
// }}}
