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
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Utils.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"

namespace LLVMGenVisitorHelpersNS
{
    /// Visitor to turn a ParamsAST to a format that the LLVMGenVisitor can use
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

    /// Visitor to turn a TypeAST to a format that the LLVMGenVisitor can use
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

    /// Visitor to declare functions so that forward declarations are not necessary
    class ForwDeclGenVisitor : public BlankVisitor
    {
    public:
        ForwDeclGenVisitor(llvm::Module *module_, ParamsVisitor *paramsVisitor, TypeVisitor *typeVisitor, File sourcefile, bool &errored);
        void visitFunctionAST(const FunctionAST *ast) override;

        llvm::Module *module_;
        ParamsVisitor *paramsVisitor;
        TypeVisitor *typeVisitor;
        File sourcefile;
        bool &errored;
    };
}

/// Visitor to compile ASTs to LLVM IR/object code
class LLVMGenVisitor : public Visitor
{
public:
    /// The constructor
    LLVMGenVisitor(File &sourcefile);

    // GENLLVMVISITMETHOD START
    /// Visit the BinaryAST and compile it to LLVM IR/object code
    /// @param ast The ast to visit
    void visitBinaryAST(const BinaryAST *ast) override;
    /// Visit the TernaryOpAST and compile it to LLVM IR/object code
    /// @param ast The ast to visit
    void visitTernaryOpAST(const TernaryOpAST *ast) override;
    /// Visit the UnaryAST and compile it to LLVM IR/object code
    /// @param ast The ast to visit
    void visitUnaryAST(const UnaryAST *ast) override;
    /// Visit the PrimaryAST and compile it to LLVM IR/object code
    /// @param ast The ast to visit
    void visitPrimaryAST(const PrimaryAST *ast) override;
    /// Visit the ExprStmtAST and compile it to LLVM IR/object code
    /// @param ast The ast to visit
    void visitExprStmtAST(const ExprStmtAST *ast) override;
    /// Visit the ProgramAST and compile it to LLVM IR/object code
    /// @param ast The ast to visit
    void visitProgramAST(const ProgramAST *ast) override;
    /// Visit the FunctionAST and compile it to LLVM IR/object code
    /// @param ast The ast to visit
    void visitFunctionAST(const FunctionAST *ast) override;
    /// Visit the BlockAST and compile it to LLVM IR/object code
    /// @param ast The ast to visit
    void visitBlockAST(const BlockAST *ast) override;
    /// Visit the TypeAST and compile it to LLVM IR/object code
    /// @param ast The ast to visit
    void visitTypeAST(const TypeAST *ast) override;
    /// Visit the ParamAST and compile it to LLVM IR/object code
    /// @param ast The ast to visit
    void visitParamAST(const ParamAST *ast) override;
    /// Visit the ParamsAST and compile it to LLVM IR/object code
    /// @param ast The ast to visit
    void visitParamsAST(const ParamsAST *ast) override;
    /// Visit the VarStmtAST and compile it to LLVM IR/object code
    /// @param ast The ast to visit
    void visitVarStmtAST(const VarStmtAST *ast) override;
    /// Visit the AssignAST and compile it to LLVM IR/object code
    /// @param ast The ast to visit
    void visitAssignAST(const AssignAST *ast) override;
    /// Visit the VariableRefAST and compile it to LLVM IR/object code
    /// @param ast The ast to visit
    void visitVariableRefAST(const VariableRefAST *ast) override;
    /// Visit the ReturnStmtAST and compile it to LLVM IR/object code
    /// @param ast The ast to visit
    void visitReturnStmtAST(const ReturnStmtAST *ast) override;
    /// Visit the ArgAST and compile it to LLVM IR/object code
    /// @param ast The ast to visit
    void visitArgAST(const ArgAST *ast) override;
    /// Visit the ArgsAST and compile it to LLVM IR/object code
    /// @param ast The ast to visit
    void visitArgsAST(const ArgsAST *ast) override;
    /// Visit the CallAST and compile it to LLVM IR/object code
    /// @param ast The ast to visit
    void visitCallAST(const CallAST *ast) override;
    // GENLLVMVISITMETHOD END

private:
    /// Create an alloca instruction at the entry of a function
    /// @param f The function to put the instruction into
    /// @param name The name of the alloca
    llvm::AllocaInst* createEntryAlloca(llvm::Function *f, const std::string &name);
    /// Create a new scope
    void beginNewScope();
    /// Finish the current scope and clean out variables in the scope from scopesymbols
    void finishCurScope();
    /// Get an AllocaInst corrosponding to a variable by searching through scopesymbols
    /// @param name The name of the variable
    /// @param tok The token to error at in case the variable is not found
    /// @param overrideErr Whether or not to override displaying the error
    llvm::AllocaInst* getVarFromName(std::string &name, Token const &tok, bool overrideErr=false);
    /// Create an entry in scopesymbols with a name and the current scope
    /// @param name The name to register this entry under
    /// @param alloca The AllocaInst to register
    void createScopeSymbol(std::string &name, llvm::AllocaInst* alloca);

    /// The LLVM context to use for functions
    llvm::LLVMContext context;
    /// The builder to create instructions
    llvm::IRBuilder<> builder;
    /// The module to put instructions into
    std::unique_ptr<llvm::Module> module_;
    /// The FunctionPassManager to manage optimization passes
    std::unique_ptr<llvm::legacy::FunctionPassManager> fpm;
    /// The map between variable names and a scope and an alloca
    std::map<std::pair<int, std::string>, llvm::AllocaInst*> scopesymbols;
    /// The current scope number
    int scopenum;
    /// Whether or not the visitor has errored in trying to compile this code
    bool errored;

    /// Throw an error and set the errored member
    /// @param t The token to error at
    /// @param message The error message to show
    /// @param sourcefile The file being compiled
    void error(Token const &t, std::string const &message, File const &sourcefile);

    /// The current return value because the visiting methods return void
    llvm::Value *curRetVal = nullptr;

    /// The file being compiled
    File &sourcefile;
    /// The parameter visitor to process ParamsAST
    LLVMGenVisitorHelpersNS::ParamsVisitor paramsVisitor;
    /// The type visitor to process TypeAST
    LLVMGenVisitorHelpersNS::TypeVisitor typeVisitor;
    /// A visitor to declare all the functions so that forward declarations are not necessary in the source file 
    LLVMGenVisitorHelpersNS::ForwDeclGenVisitor forwDeclVisitor;
};

