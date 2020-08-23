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

class LLVMGenVisitor : public Visitor
{
public:
    LLVMGenVisitor(File &sourcefile);

    void visitBinaryAST(const BinaryAST *ast);
    void visitTernaryOpAST(const TernaryOpAST *ast);
    void visitUnaryAST(const UnaryAST *ast);
    void visitPrimaryAST(const PrimaryAST *ast);
    void visitExprStmtAST(const ExprStmtAST *ast);
    void visitProgramAST(const ProgramAST *ast);
    void visitFunctionAST(const FunctionAST *ast);
    void visitBlockAST(const BlockAST *ast);
    void visitTypeAST(const TypeAST *ast);
    void visitArgAST(const ArgAST *ast);
    void visitArgsAST(const ArgsAST *ast);
    void visitVarStmtAST(const VarStmtAST *ast);

private:
    llvm::AllocaInst* createEntryAlloca(llvm::Function *f, const std::string &name);
    llvm::LLVMContext context;
    llvm::IRBuilder<> builder;
    std::unique_ptr<llvm::Module> module_;
    std::map<std::string, llvm::AllocaInst*> scopesymbols;

    llvm::Value *curRetVal = nullptr;

    File &sourcefile;
};
