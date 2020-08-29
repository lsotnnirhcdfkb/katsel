/// @file llvmgenvisitor.cpp
/// LLVMGenVisitor method definitions
/// All the compiling work is in these methods.

#include "llvmgenvisitor.h"

/// Convenience macro to "return" a value from a visiting method, because each visiting method returns void
#define LLVMGENVISITOR_RETURN(x) curRetVal = x; \
                                             return;
/// Convenience macro to clear the return value because at the start of a new function call, the return value must be cleared
#define CLEARRET curRetVal = nullptr

LLVMGenVisitor::LLVMGenVisitor(File &sourcefile): sourcefile(sourcefile), builder(context), module_(std::make_unique<llvm::Module>("COxianc output of file " + sourcefile.filename, context)), scopenum(0), paramsVisitor(sourcefile, context), typeVisitor(sourcefile, context), forwDeclVisitor(module_.get(), &paramsVisitor, &typeVisitor, sourcefile, errored), fpm(std::make_unique<llvm::legacy::FunctionPassManager>(module_.get())), errored(false)
{
    fpm->add(llvm::createPromoteMemoryToRegisterPass());
    fpm->add(llvm::createInstructionCombiningPass());
    fpm->add(llvm::createReassociatePass());
    fpm->add(llvm::createGVNPass());
    fpm->add(llvm::createCFGSimplificationPass());

    fpm->doInitialization();
}

// {{{ visiting asts
// {{{ programast
void LLVMGenVisitor::visitProgramAST(const ASTs::ProgramAST *ast) 
{
    CLEARRET;

    for (const std::unique_ptr<ASTs::AST> &dast : ast->asts) 
    {
        dast->accept(&forwDeclVisitor);
    }

    for (const std::unique_ptr<ASTs::AST> &dast : ast->asts) 
    {
        dast->accept(this);
    }

    // module_->print(llvm::outs(), nullptr);
    if (errored)
        return;

    auto targetTriple = llvm::sys::getDefaultTargetTriple();

    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();

    std::string errmsg;
    auto target = llvm::TargetRegistry::lookupTarget(targetTriple, errmsg);

    if (!target)
    {
        llvm::errs() << errmsg;
        return;
    }

    auto cpu = "generic";
    auto features = "";

    llvm::TargetOptions opt;
    auto rm = llvm::Optional<llvm::Reloc::Model>();
    auto targetMachine = target->createTargetMachine(targetTriple, cpu, features, opt, rm);

    module_->setDataLayout(targetMachine->createDataLayout());
    module_->setTargetTriple(targetTriple);

    auto filename = "out.o";
    std::error_code errc;

    llvm::raw_fd_ostream outstream(filename, errc, llvm::sys::fs::OpenFlags::OF_None);

    if (errc)
    {
        llvm::errs() << "Could not open file \"" << filename << "\": " << errc.message();
        return;
    }

    llvm::legacy::PassManager emitpm;
    auto fileType = llvm::CGFT_ObjectFile;

    if (targetMachine->addPassesToEmitFile(emitpm, outstream, nullptr, fileType)) {
        llvm::errs() << "cannot emit this type of file";
        return;
    }

    emitpm.run(*module_);
    outstream.flush();
    outstream.close();
}
// }}}
// {{{ declaration visiting
void LLVMGenVisitor::visitFunctionAST(const ASTs::FunctionAST *ast)
{
    CLEARRET;

    std::string name = std::string(ast->name.start, ast->name.end);

    llvm::Function *f = module_->getFunction(name);
    // f should already be defined beacuse of the forwdeclgenvisitor
    // but it should be empty becuase that only generated the declarations
    // not the body
    if (!f->empty())
    {
        // so if it is not empty then this is a redefinition, but
        // don't report an error since forwdeclgenvisitor already 
        // did that
        LLVMGENVISITOR_RETURN(nullptr)
    }

    if (!ast->body)
    {
        // just a declaration
        LLVMGENVISITOR_RETURN(nullptr)
    }

    llvm::BasicBlock *block = llvm::BasicBlock::Create(context, name + "entry", f);
    builder.SetInsertPoint(block);

    for (auto &param : f->args())
    {
        llvm::AllocaInst *alloca = createEntryAlloca(f, param.getName());
        builder.CreateStore(&param, alloca);
        std::string pname = param.getName();
        createScopeSymbol(pname, alloca);
    }

    beginNewScope();

    ast->body->accept(this);

    llvm::verifyFunction(*f); 

    finishCurScope();

    if (!errored)
        fpm->run(*f);

    LLVMGENVISITOR_RETURN(f);
}
// }}}
// {{{ expression visiting
// {{{ binary ast
void LLVMGenVisitor::visitBinaryAST(const ASTs::BinaryAST *ast) 
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

        default: error(ast->op, "invalid thingy", sourcefile); retval = nullptr; // shouldn't ever get here
    }

    LLVMGENVISITOR_RETURN(retval);
}
// }}}
// {{{ ternary ast
void LLVMGenVisitor::visitTernaryOpAST(const ASTs::TernaryOpAST *ast) 
{
    CLEARRET;
    ast->conditional->accept(this);
    llvm::Value *cond = curRetVal;

    cond = builder.CreateICmpNE(cond, llvm::ConstantInt::get(context, llvm::APInt(1, 0)));

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
void LLVMGenVisitor::visitUnaryAST(const ASTs::UnaryAST *ast) 
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

        default: error(ast->op, "invalid thingy", sourcefile); retval = nullptr; // shouldn't ever get here
    }

    LLVMGENVISITOR_RETURN(retval)
}
// }}}
// {{{ assign ast
void LLVMGenVisitor::visitAssignAST(const ASTs::AssignAST *ast)
{
    CLEARRET;
    ASTs::VariableRefAST *lhs = dynamic_cast<ASTs::VariableRefAST*>(ast->lhs.get());

    if (!lhs)
    {
        error(ast->equalSign, "Invalid tparamet for assignment", sourcefile);
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
void LLVMGenVisitor::visitVariableRefAST(const ASTs::VariableRefAST *ast)
{
    CLEARRET;
    std::string name = std::string(ast->var.start, ast->var.end);

    // find variable with override because it could
    // be a function and not be in the variable map
    llvm::AllocaInst *v = getVarFromName(name, ast->var, true);

    if (v)
    {
        LLVMGENVISITOR_RETURN(builder.CreateLoad(v, name.c_str()))
    }

    // not a variable so could be function
    llvm::Function *f = module_->getFunction(name);

    if (f)
    {
        LLVMGENVISITOR_RETURN(f);
    }

    error(ast->var, "Unknown name", sourcefile);
    LLVMGENVISITOR_RETURN(nullptr)
}
// }}}
void LLVMGenVisitor::visitPrimaryAST(const ASTs::PrimaryAST *ast) 
{
    CLEARRET;
    LLVMGENVISITOR_RETURN(llvm::ConstantInt::get(context, llvm::APInt(64, std::stoi(std::string(ast->value.start, ast->value.end)))))
}
// }}}
// {{{ statement visiting
void LLVMGenVisitor::visitExprStmtAST(const ASTs::ExprStmtAST *ast) 
{
    CLEARRET;
    ast->ast->accept(this);
    LLVMGENVISITOR_RETURN(nullptr)
}

void LLVMGenVisitor::visitVarStmtAST(const ASTs::VarStmtAST *ast) 
{
    CLEARRET;
    // TODO: types
    std::string varname = std::string(ast->name.start, ast->name.end);

    // find variable with error override
    if (getVarFromName(varname, ast->name, true))
    {
        error(ast->name, "cannot redefine variable", sourcefile);
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
void LLVMGenVisitor::visitReturnStmtAST(const ASTs::ReturnStmtAST *ast)
{
    CLEARRET;
    if (ast->expr)
    {
        ast->expr->accept(this);
        builder.CreateRet(curRetVal);
    }
    else
    {
        builder.CreateRetVoid();
    }
}
// }}}
// {{{ helper ast visiting
void LLVMGenVisitor::visitTypeAST(const ASTs::TypeAST *ast) 
{
    CLEARRET;
}

void LLVMGenVisitor::visitBlockAST(const ASTs::BlockAST *ast) 
{
    CLEARRET;
    beginNewScope();
    for (const std::unique_ptr<ASTs::AST> &bast : ast->stmts) 
    {
        bast->accept(this);
    }
    finishCurScope();
    LLVMGENVISITOR_RETURN(nullptr)
}

void LLVMGenVisitor::visitParamAST(const ASTs::ParamAST *ast) 
{
    CLEARRET;
    // shouldn't ever happen beacause ParamsVisitor processes the params
    // instead of LLVMGenVisitor
}

void LLVMGenVisitor::visitParamsAST(const ASTs::ParamsAST *ast) 
{
    CLEARRET;
    // also shouldn't ever happen beacause ParamsVisitor processes the params
    // instead of LLVMGenVisitor
}

void LLVMGenVisitor::visitArgAST(const ASTs::ArgAST *ast) 
{
    // ASTs::visitCallAST calls this
    CLEARRET;

    ast->expr->accept(this);
    LLVMGENVISITOR_RETURN(curRetVal); // this technically doesn't do anything but whatever
}

void LLVMGenVisitor::visitArgsAST(const ASTs::ArgsAST *ast) 
{
    // this shouldnot get called because ASTs::visitCallAST parses ArgsAST
    CLEARRET;
}

void LLVMGenVisitor::visitCallAST(const ASTs::CallAST *ast) 
{
    CLEARRET;

    ast->varrefast->accept(this);
    llvm::Value *f = curRetVal;

    if (!f)
    {
        LLVMGENVISITOR_RETURN(nullptr);
    }

    if (!f->getType()->isPointerTy()) // should be a pointer to a function
    {
        error(ast->oparn, "Cannot call non-function", sourcefile);
        LLVMGENVISITOR_RETURN(nullptr);
    } else
    {
        // so it is a pointer
        // but is it a pointer to a function?
        if (!(static_cast<llvm::PointerType*>(f->getType()))->getElementType()->isFunctionTy())
        {
            // is not a pointer to a function
            // return
            error(ast->oparn, "Cannot call non-function", sourcefile);
            LLVMGENVISITOR_RETURN(nullptr);
        }
    }

    std::vector<llvm::Value*> valargs;

    if (ast->arglistast)
    {
        ASTs::ArgsAST *argsast = dynamic_cast<ASTs::ArgsAST*>(ast->arglistast.get());

        // internal parsing error, because Parser::arglist() always returns a std::unique_ptr<ArgsAST>
        if (!argsast) 
        {
            LLVMGENVISITOR_RETURN(nullptr);
        }

        // static cast is safe beacuse it was checked before
        if (static_cast<llvm::Function*>(f)->arg_size() != argsast->args.size())
        {
            error(ast->oparn, "Wrong number of arguments passed to function call", sourcefile);
            LLVMGENVISITOR_RETURN(nullptr);
        }


        for (std::unique_ptr<ASTs::AST> &expr: argsast->args)
        {
            expr->accept(this);
            llvm::Value *argvalue = curRetVal;
            valargs.push_back(argvalue);
        }
    }

    LLVMGENVISITOR_RETURN(builder.CreateCall(f, valargs));
}

// }}}
// }}}
// {{{ private llvm visitor helper methods
void LLVMGenVisitor::error(Token const &t, std::string const &message, File const &sourcefile)
{
    reportError(t, message, sourcefile);
    errored = true;
}
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

llvm::AllocaInst* LLVMGenVisitor::getVarFromName(std::string &name, Token const &tok, bool overrideErr)
{
    int highestScope = -1;
    llvm::AllocaInst *v = nullptr;
    for (auto it = scopesymbols.cbegin(); it != scopesymbols.cend(); ++it)
    {
        if (it->first.second == name && it->first.first > highestScope)
        {
            v = it->second;
            highestScope = it->first.first;
        }
    }

    if (!v && !overrideErr)
        error(tok, "unknown variable name", sourcefile);

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
    ParamsVisitor::ParamsVisitor(File &sourcefile, llvm::LLVMContext &context): sourcefile(sourcefile), context(context) {}
    TypeVisitor::TypeVisitor(File &sourcefile, llvm::LLVMContext &context): sourcefile(sourcefile), context(context) {}

    void ParamsVisitor::visitParamAST(const ASTs::ParamAST *ast)
    {
        // if this is part of a visitParams then this will be overrided anyway
        // but if it is not then the return value is provided in a vector like it's supposed to be
        paramTypes = {llvm::Type::getInt64Ty(context)}; 
        paramNames = {ast->paramname};
    }
    void ParamsVisitor::visitParamsAST(const ASTs::ParamsAST *ast)
    {
        std::vector<llvm::Type*> cparamTypes;
        std::vector<Token> cparamNames;

        for (std::unique_ptr<ASTs::AST> const &param : ast->params)
        {
            param->accept(this);
            // retval is length 1 because ASTs::visitParamsAST always does that
            cparamTypes.push_back(paramTypes[0]);
            cparamNames.push_back(paramNames[0]);
        }

        paramTypes = cparamTypes;
        paramNames = cparamNames;
    }

    void TypeVisitor::visitTypeAST(const ASTs::TypeAST *ast)
    {
        // right now only int64s are supported
        // TODO: types
        rettype = llvm::Type::getInt64Ty(context);
        // rettype = llvm::Type::getVoidTy(context);
    }

    ForwDeclGenVisitor::ForwDeclGenVisitor(llvm::Module *module_, ParamsVisitor *paramsVisitor, TypeVisitor *typeVisitor, File sourcefile, bool &errored): module_(module_), paramsVisitor(paramsVisitor), typeVisitor(typeVisitor), sourcefile(sourcefile), errored(errored) {}

    void ForwDeclGenVisitor::visitFunctionAST(const ASTs::FunctionAST *ast)
    {
        std::string name = std::string(ast->name.start, ast->name.end);

        llvm::Function *fcheck = module_->getFunction(name);
        // if the function was there then it is being redefined
        if (fcheck)
        {
            reportError(ast->name, "Cannot redefine function", sourcefile);
            errored = true;
            return;
        }

        std::vector<llvm::Type*> paramTypes;
        std::vector<Token> paramNames;

        if (ast->params)
        {
            ast->params->accept(paramsVisitor);

            paramTypes = paramsVisitor->paramTypes;
            paramNames = paramsVisitor->paramNames;
        }

        ast->type->accept(typeVisitor);
        llvm::Type *rettype = typeVisitor->rettype;
        llvm::FunctionType *ft = llvm::FunctionType::get(rettype, paramTypes, false); 
        llvm::Function *f = llvm::Function::Create(ft, llvm::Function::ExternalLinkage, name, *module_);

        { 
            int i = 0;
            for (auto &param : f->args())
            {
                std::string paramName = std::string(paramNames[i].start, paramNames[i].end);
                param.setName(paramName);
            }

        }
    }

}
// }}}
