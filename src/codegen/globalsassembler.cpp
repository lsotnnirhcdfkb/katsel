#include "codegen/globalsassembler.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/DerivedTypes.h"

#include "message/errors.h"

#include <iostream>

GlobalsAssembler::GlobalsAssembler(CodeGenContext &con, CodeGen &codeGen): context(con), codeGen(codeGen) {}

void GlobalsAssembler::visitDecls(ASTNS::Decls *a)
{
    a->decls->accept(this);
    a->decl->accept(this);
}

void GlobalsAssembler::visitFunction(ASTNS::Function *a)
{
    std::string fnamestr (a->name.stringify());
    Value declbefore = context.getGlobal(fnamestr);
    if (declbefore.val)
    {
        Error(Error::MsgType::ERROR, a->name, "Duplicate function")
            .underline(Error::Underline(a->name, '^')
                .error("Duplciate function"))
            .underline(Error::Underline(declbefore, '-')
                .note("Previous declaration is here"))
            .report();
        return;
    }

    Type *ret = codeGen.evalType(a->retty.get());

    std::vector<CodeGen::Param> params;
    if (a->form == ASTNS::Function::Form::FUN_RETTY_NAME_OPARN_PARAMLIST_CPARN_BODY)
        params = codeGen.evalParams(a->paramlist.get());

    std::vector<llvm::Type*> paramtysasllvm;
    std::vector<Type*> paramtys;

    for (CodeGen::Param p : params)
    {
        paramtys.push_back(p.ty);
        paramtysasllvm.push_back(p.ty->toLLVMType(context.context));
    }

    llvm::Type *retTyllvm = ret->toLLVMType(context.context);
    llvm::FunctionType *ft = llvm::FunctionType::get(retTyllvm, paramtysasllvm, false);
    llvm::Function *f = llvm::Function::Create(ft, llvm::Function::ExternalLinkage, a->name.stringify(), context.mod.get());

    context.globalSymbolTable[fnamestr] = Value(context.getFunctionType(ret, paramtys), f, a);
}

void GlobalsAssembler::visitAdditionexpr(ASTNS::Additionexpr *ast) {}
void GlobalsAssembler::visitArgs(ASTNS::Args *ast) {}
void GlobalsAssembler::visitAssignmentexpr(ASTNS::Assignmentexpr *ast) {}
void GlobalsAssembler::visitBinandexpr(ASTNS::Binandexpr *ast) {}
void GlobalsAssembler::visitBinnotexpr(ASTNS::Binnotexpr *ast) {}
void GlobalsAssembler::visitBinorexpr(ASTNS::Binorexpr *ast) {}
void GlobalsAssembler::visitBitandexpr(ASTNS::Bitandexpr *ast) {}
void GlobalsAssembler::visitBitorexpr(ASTNS::Bitorexpr *ast) {}
void GlobalsAssembler::visitBitshiftexpr(ASTNS::Bitshiftexpr *ast) {}
void GlobalsAssembler::visitBitxorexpr(ASTNS::Bitxorexpr *ast) {}
void GlobalsAssembler::visitBlock(ASTNS::Block *ast) {}
void GlobalsAssembler::visitCallexpr(ASTNS::Callexpr *ast) {}
void GlobalsAssembler::visitCompeqexpr(ASTNS::Compeqexpr *ast) {}
void GlobalsAssembler::visitComplgtexpr(ASTNS::Complgtexpr *ast) {}
void GlobalsAssembler::visitDecl(ASTNS::Decl *ast) {}
void GlobalsAssembler::visitExpression(ASTNS::Expression *ast) {}
void GlobalsAssembler::visitExprstmt(ASTNS::Exprstmt *ast) {}
void GlobalsAssembler::visitMultexpr(ASTNS::Multexpr *ast) {}
void GlobalsAssembler::visitParamlist(ASTNS::Paramlist *ast) {}
void GlobalsAssembler::visitPrimaryexpr(ASTNS::Primaryexpr *ast) {}
void GlobalsAssembler::visitRetstmt(ASTNS::Retstmt *ast) {}
void GlobalsAssembler::visitStmt(ASTNS::Stmt *ast) {}
void GlobalsAssembler::visitStmts(ASTNS::Stmts *ast) {}
void GlobalsAssembler::visitTernaryexpr(ASTNS::Ternaryexpr *ast) {}
void GlobalsAssembler::visitType(ASTNS::Type *ast) {}
void GlobalsAssembler::visitUnaryexpr(ASTNS::Unaryexpr *ast) {}
void GlobalsAssembler::visitVarstmt(ASTNS::Varstmt *ast) {}
void GlobalsAssembler::visitVarstmtitems(ASTNS::Varstmtitems *ast) {}
void GlobalsAssembler::visitVarstmtitem(ASTNS::Varstmtitem *ast) {}
void GlobalsAssembler::visitEmptystmt(ASTNS::Emptystmt *ast) {}
