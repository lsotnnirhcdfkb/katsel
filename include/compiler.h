/// @file compiler.h
/// LLVM Compiling visior classes

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

/// Namespace for Compiler and it's helper visitors and stuff
namespace CompilerNS
{
    /// Visitor to turn a ParamsAST to a format that the Compiler can use
    class ParamsVisitor : public BlankVisitor
    {
    public:
        /// The constructor
        /// @param sourcefile The source file
        /// @param context The LLVM context that this ParamsVisitor can use
        ParamsVisitor(File &sourcefile, llvm::LLVMContext &context);

        /// Visit a ParamAST and create an appropriate return value for it
        /// @param ast The parameter ast to visit
        void visitParamAST(const ASTs::ParamAST *ast) override;
        /// Visit a ParamsAST and create an appropriate return value for it
        /// @param ast The parameters ast to visit
        void visitParamsAST(const ASTs::ParamsAST *ast) override;

        /// A return vector of type that the parameters are
        std::vector<llvm::Type*> paramTypes;
        /// A return vector of names that the parameters are
        std::vector<Token> paramNames;

    private:
        /// The source file
        File &sourcefile;
        /// The LLVM Context that this ParamsVisitor can use
        llvm::LLVMContext &context;
    };

    /// Visitor to turn a TypeAST to a format that the Compiler can use
    class TypeVisitor : public BlankVisitor
    {
    public:
        /// The constructor
        /// @param sourcefile The source file
        /// @param context The LLVM context that this TypeVisitor can use
        TypeVisitor(File &sourcefile, llvm::LLVMContext &context);

        /// Visit a TypeAST and turn it into an LLVM Type
        /// @param ast The AST to convert into an LLVM Type
        void visitTypeAST(const ASTs::TypeAST *ast) override;

        /// The return type that was created by this visitor
        llvm::Type *rettype;

    private:
        /// The source file
        File &sourcefile;
        /// The LLVM Context that this TypeVisitor can use
        llvm::LLVMContext &context;
    };

    /// Visitor to declare functions so that forward declarations are not necessary
    class ForwDeclGenVisitor : public BlankVisitor
    {
    public:
        /// The constructor
        /// @param module_ The module to generate to
        /// @param paramsVisitor The parameter visitor to process parameters
        /// @param typeVisitor The type visitor to process the return type of funcion declarations
        /// @param sourcefile The sourcefile that is being compiled
        /// @param errored A reference to the Compiler errored variable so that this visitor can set the Compiler::errored flag
        ForwDeclGenVisitor(llvm::Module *module_, ParamsVisitor *paramsVisitor, TypeVisitor *typeVisitor, File sourcefile, bool &errored);
        /// Visit a function AST and create a forward declaration for it
        /// @param ast The function AST to visit
        void visitFunctionAST(const ASTs::FunctionAST *ast) override;

        /// The module to generate forward declarations into
        llvm::Module *module_;
        /// The parameter visitor to process parameters
        ParamsVisitor *paramsVisitor;
        /// The type visitor to process the return type of funcion declarations
        TypeVisitor *typeVisitor;
        /// The sourcefile that is being compiled
        File sourcefile;
        /// A reference to the Compiler errored variable so that this visitor can set the Compiler::errored flag
        bool &errored;
    };

    /// Visitor to compile ASTs to LLVM IR/object code
    class Compiler : public Visitor
    {
    public:
        /// The constructor
        Compiler(File &sourcefile);

        // GENLLVMVISITMETHOD START
        /// Visit the BinaryAST and compile it to LLVM IR/object code
        /// @param ast The ast to visit
        void visitBinaryAST(const ASTs::BinaryAST *ast) override;
        /// Visit the TernaryOpAST and compile it to LLVM IR/object code
        /// @param ast The ast to visit
        void visitTernaryOpAST(const ASTs::TernaryOpAST *ast) override;
        /// Visit the UnaryAST and compile it to LLVM IR/object code
        /// @param ast The ast to visit
        void visitUnaryAST(const ASTs::UnaryAST *ast) override;
        /// Visit the PrimaryAST and compile it to LLVM IR/object code
        /// @param ast The ast to visit
        void visitPrimaryAST(const ASTs::PrimaryAST *ast) override;
        /// Visit the ExprStmtAST and compile it to LLVM IR/object code
        /// @param ast The ast to visit
        void visitExprStmtAST(const ASTs::ExprStmtAST *ast) override;
        /// Visit the ProgramAST and compile it to LLVM IR/object code
        /// @param ast The ast to visit
        void visitProgramAST(const ASTs::ProgramAST *ast) override;
        /// Visit the FunctionAST and compile it to LLVM IR/object code
        /// @param ast The ast to visit
        void visitFunctionAST(const ASTs::FunctionAST *ast) override;
        /// Visit the BlockAST and compile it to LLVM IR/object code
        /// @param ast The ast to visit
        void visitBlockAST(const ASTs::BlockAST *ast) override;
        /// Visit the TypeAST and compile it to LLVM IR/object code
        /// @param ast The ast to visit
        void visitTypeAST(const ASTs::TypeAST *ast) override;
        /// Visit the ParamAST and compile it to LLVM IR/object code
        /// @param ast The ast to visit
        void visitParamAST(const ASTs::ParamAST *ast) override;
        /// Visit the ParamsAST and compile it to LLVM IR/object code
        /// @param ast The ast to visit
        void visitParamsAST(const ASTs::ParamsAST *ast) override;
        /// Visit the VarStmtAST and compile it to LLVM IR/object code
        /// @param ast The ast to visit
        void visitVarStmtAST(const ASTs::VarStmtAST *ast) override;
        /// Visit the AssignAST and compile it to LLVM IR/object code
        /// @param ast The ast to visit
        void visitAssignAST(const ASTs::AssignAST *ast) override;
        /// Visit the VariableRefAST and compile it to LLVM IR/object code
        /// @param ast The ast to visit
        void visitVariableRefAST(const ASTs::VariableRefAST *ast) override;
        /// Visit the LValueAST and compile it to LLVM IR/object code
        /// @param ast The ast to visit
        void visitLValueAST(const ASTs::LValueAST *ast) override;
        /// Visit the ReturnStmtAST and compile it to LLVM IR/object code
        /// @param ast The ast to visit
        void visitReturnStmtAST(const ASTs::ReturnStmtAST *ast) override;
        /// Visit the ArgAST and compile it to LLVM IR/object code
        /// @param ast The ast to visit
        void visitArgAST(const ASTs::ArgAST *ast) override;
        /// Visit the ArgsAST and compile it to LLVM IR/object code
        /// @param ast The ast to visit
        void visitArgsAST(const ASTs::ArgsAST *ast) override;
        /// Visit the CallAST and compile it to LLVM IR/object code
        /// @param ast The ast to visit
        void visitCallAST(const ASTs::CallAST *ast) override;
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
        std::map<std::pair<size_t, std::string>, llvm::AllocaInst*> scopesymbols;
        /// The current scope number
        size_t scopenum;
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
        ParamsVisitor paramsVisitor;
        /// The type visitor to process TypeAST
        TypeVisitor typeVisitor;
        /// A visitor to declare all the functions so that forward declarations are not necessary in the source file 
        ForwDeclGenVisitor forwDeclVisitor;
    };
}

void compile(ASTs::AST *ast, File &sourcefile);

