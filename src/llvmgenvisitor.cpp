#include "llvmgenvisitor.h"

LLVMGenVisitor::LLVMGenVisitor(std::string &source): source(source), builder(context), module_(std::make_unique<llvm::Module>("cool", context)) {}

void LLVMGenVisitor::visitBinaryAST(const BinaryAST *ast) {
    std::cout << "in visitBinaryast" << std::endl;
    ast->last->accept(this);
    llvm::Value *lval = curRetVal;
    ast->rast->accept(this);
    llvm::Value *rval = curRetVal;

    if (!lval || !rval) {
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

        default: reportError(ast->op, "invalid thingy", source); retval = nullptr;
    }

    curRetVal = retval;
}

void LLVMGenVisitor::visitTernaryOpAST(const TernaryOpAST *ast) {
    ast->conditional->accept(this);
    Value *cond = curRetVal;

    cond = Builder.CreateFCmpONE(cond, llvm::ConstantInt::get(context, llvm::APInt(0));
    
    llvm::Function *func = builder.GetInsertBlock()->getParent();

    auto *trueb = BasicBlock::Create(context, "trueblock", func);
    auto *falseb = BasicBlock::Create(context, "falseblock");
    auto *afterb = BasicBlock::Create(context, "afterblock");

    builder.CreateCondBr(cond, thenb, elseb);
    builder.SetInsertPoint(trueb);

    ast->trueast->accept(this);
    Value *truev = curRetVal;

    builder.createBr(afterb);
    trueb = Builder.GetInsertBlock();

    func->getBasicBlockList().push_back(falseb);
    builder.setInsertPoint(falseb);

    ast->falseast->accept(this);
    Value *falsev = curRetVal;

    Builder.createBr(afterb);
    falseb = Builder.GetInsertBlock();

    func->getBasicBlockList().push_back(afterb);
    Builder.SetInsertPoint(afterb);
    PHINode *phi = Builder.CreatePHI(Type::getIntTy(context), 2);

    phi->addIncoming(truev, trueb);
    phi->addIncoming(elsev, elseb);

    curRetVal = phi;
}

void LLVMGenVisitor::visitUnaryAST(const UnaryAST *ast) {
}

void LLVMGenVisitor::visitPrimaryAST(const PrimaryAST *ast) {
    std::cout << "in visitPrimaryast" << std::endl;
    curRetVal = llvm::ConstantInt::get(context, llvm::APInt(8, std::stoi(std::string(ast->value.start, ast->value.end))));
    // curRetVal = llvm::ConstantFP::get(context, llvm::APFloat((float) std::stoi(std::string(ast->value.start, ast->value.end))));
}

void LLVMGenVisitor::visitExprStmtAST(const ExprStmtAST *ast) {
    std::cout << "in visitexprstmtast" << std::endl;
    ast->ast->accept(this);
}

void LLVMGenVisitor::visitProgramAST(const ProgramAST *ast) {
    llvm::FunctionType *ft = llvm::FunctionType::get(llvm::Type::getVoidTy(context), false); 
    llvm::Function *f = llvm::Function::Create(ft, llvm::Function::ExternalLinkage, "Anonymous", *module_);

    llvm::BasicBlock *block = llvm::BasicBlock::Create(context, "anonymousblock", f);
    builder.SetInsertPoint(block);

    for (const std::unique_ptr<AST> &sast : ast->asts) {
        sast->accept(this);
    }

    builder.CreateRetVoid();
    llvm::verifyFunction(*f);

    std::cout << "inifhs" << std::endl;

    module_->print(llvm::outs(), nullptr);
}
