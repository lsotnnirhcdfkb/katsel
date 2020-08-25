#include <map>
#include <string>
#include <memory>

#include "file.h"
#include "visitor.h"
#include "ast.h"
#include "errors.h"

#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"

namespace LLVMGenVisitorHelpersNS
{
    class ParamsVisitor : public BlankVisitor
    {
    public:
        ParamsVisitor(File &sourcefile, llvm::LLVMContext &context);

        void visitParamAST(const ParamAST *ast) override;
        void visitParamsAST(const ParamsAST *ast) override;

        std::vector<llvm::Type*> paramTypes;
        std::vector<Token> paramNames;

    private:
        File &sourcefile;
        llvm::LLVMContext &context;
    };

    class TypeVisitor : public BlankVisitor
    {
    public:
        TypeVisitor(File &sourcefile, llvm::LLVMContext &context);

        void visitTypeAST(const TypeAST *ast) override;

        llvm::Type *rettype;

    private:
        File &sourcefile;
        llvm::LLVMContext &context;
    };
}

class LLVMGenVisitor : public Visitor
{
public:
    LLVMGenVisitor(File &sourcefile);

    // GENVISITORMETHOD3 START
    void visitBinaryAST(const BinaryAST *ast) override;
    void visitTernaryOpAST(const TernaryOpAST *ast) override;
    void visitUnaryAST(const UnaryAST *ast) override;
    void visitPrimaryAST(const PrimaryAST *ast) override;
    void visitExprStmtAST(const ExprStmtAST *ast) override;
    void visitProgramAST(const ProgramAST *ast) override;
    void visitFunctionAST(const FunctionAST *ast) override;
    void visitBlockAST(const BlockAST *ast) override;
    void visitTypeAST(const TypeAST *ast) override;
    void visitParamAST(const ParamAST *ast) override;
    void visitParamsAST(const ParamsAST *ast) override;
    void visitVarStmtAST(const VarStmtAST *ast) override;
    void visitAssignAST(const AssignAST *ast) override;
    void visitVariableRefAST(const VariableRefAST *ast) override;
    void visitReturnStmtAST(const ReturnStmtAST *ast) override;
    void visitArgAST(const ArgAST *ast) override;
    void visitArgsAST(const ArgsAST *ast) override;
    void visitCallAST(const CallAST *ast) override;
    // GENVISITORMETHOD3 END

private:
    llvm::AllocaInst* createEntryAlloca(llvm::Function *f, const std::string &name);
    void beginNewScope();
    void finishCurScope();
    llvm::AllocaInst* getVarFromName(std::string &name, Token const &tok, bool overrideErr=false);
    void createScopeSymbol(std::string &name, llvm::AllocaInst* alloca);

    llvm::LLVMContext context;
    llvm::IRBuilder<> builder;
    std::unique_ptr<llvm::Module> module_;
    std::map<std::pair<int, std::string>, llvm::AllocaInst*> scopesymbols;
    int scopenum;

    llvm::Value *curRetVal = nullptr;

    File &sourcefile;
    LLVMGenVisitorHelpersNS::ParamsVisitor paramsVisitor;
    LLVMGenVisitorHelpersNS::TypeVisitor typeVisitor;
};

