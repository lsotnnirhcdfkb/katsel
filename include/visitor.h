#pragma once

#include <string>

// GENFORWARDDECL START
class AST;
class BinaryAST;
class TernaryOpAST;
class UnaryAST;
class PrimaryAST;
class ExprStmtAST;
class ProgramAST;
class FunctionAST;
class BlockAST;
class TypeAST;
class ParamAST;
class ParamsAST;
class VarStmtAST;
class AssignAST;
class VariableRefAST;
class ReturnStmtAST;
class ArgAST;
class ArgsAST;
class CallAST;
// GENFORWARDDECL END

/// Blank pure virtual visitor class that other visitors can extend
class Visitor
{
public:
    // GENVISITORMETHODBASE START
    /// Visit a(n) BinaryAST 
    /// @param ast The ast to visit
    virtual void visitBinaryAST(const BinaryAST *ast) = 0;
    /// Visit a(n) TernaryOpAST 
    /// @param ast The ast to visit
    virtual void visitTernaryOpAST(const TernaryOpAST *ast) = 0;
    /// Visit a(n) UnaryAST 
    /// @param ast The ast to visit
    virtual void visitUnaryAST(const UnaryAST *ast) = 0;
    /// Visit a(n) PrimaryAST 
    /// @param ast The ast to visit
    virtual void visitPrimaryAST(const PrimaryAST *ast) = 0;
    /// Visit a(n) ExprStmtAST 
    /// @param ast The ast to visit
    virtual void visitExprStmtAST(const ExprStmtAST *ast) = 0;
    /// Visit a(n) ProgramAST 
    /// @param ast The ast to visit
    virtual void visitProgramAST(const ProgramAST *ast) = 0;
    /// Visit a(n) FunctionAST 
    /// @param ast The ast to visit
    virtual void visitFunctionAST(const FunctionAST *ast) = 0;
    /// Visit a(n) BlockAST 
    /// @param ast The ast to visit
    virtual void visitBlockAST(const BlockAST *ast) = 0;
    /// Visit a(n) TypeAST 
    /// @param ast The ast to visit
    virtual void visitTypeAST(const TypeAST *ast) = 0;
    /// Visit a(n) ParamAST 
    /// @param ast The ast to visit
    virtual void visitParamAST(const ParamAST *ast) = 0;
    /// Visit a(n) ParamsAST 
    /// @param ast The ast to visit
    virtual void visitParamsAST(const ParamsAST *ast) = 0;
    /// Visit a(n) VarStmtAST 
    /// @param ast The ast to visit
    virtual void visitVarStmtAST(const VarStmtAST *ast) = 0;
    /// Visit a(n) AssignAST 
    /// @param ast The ast to visit
    virtual void visitAssignAST(const AssignAST *ast) = 0;
    /// Visit a(n) VariableRefAST 
    /// @param ast The ast to visit
    virtual void visitVariableRefAST(const VariableRefAST *ast) = 0;
    /// Visit a(n) ReturnStmtAST 
    /// @param ast The ast to visit
    virtual void visitReturnStmtAST(const ReturnStmtAST *ast) = 0;
    /// Visit a(n) ArgAST 
    /// @param ast The ast to visit
    virtual void visitArgAST(const ArgAST *ast) = 0;
    /// Visit a(n) ArgsAST 
    /// @param ast The ast to visit
    virtual void visitArgsAST(const ArgsAST *ast) = 0;
    /// Visit a(n) CallAST 
    /// @param ast The ast to visit
    virtual void visitCallAST(const CallAST *ast) = 0;
    // GENVISITORMETHODBASE END
};

/// A visitor prints out each AST in a readable format
class PrintVisitor : public Visitor
{
public:
    PrintVisitor();
    // GENPRINTVISITMETHOD START
    /// Visit the BinaryAST and print it in a readable form
    /// @param ast The ast to visit
    void visitBinaryAST(const BinaryAST *ast) override;
    /// Visit the TernaryOpAST and print it in a readable form
    /// @param ast The ast to visit
    void visitTernaryOpAST(const TernaryOpAST *ast) override;
    /// Visit the UnaryAST and print it in a readable form
    /// @param ast The ast to visit
    void visitUnaryAST(const UnaryAST *ast) override;
    /// Visit the PrimaryAST and print it in a readable form
    /// @param ast The ast to visit
    void visitPrimaryAST(const PrimaryAST *ast) override;
    /// Visit the ExprStmtAST and print it in a readable form
    /// @param ast The ast to visit
    void visitExprStmtAST(const ExprStmtAST *ast) override;
    /// Visit the ProgramAST and print it in a readable form
    /// @param ast The ast to visit
    void visitProgramAST(const ProgramAST *ast) override;
    /// Visit the FunctionAST and print it in a readable form
    /// @param ast The ast to visit
    void visitFunctionAST(const FunctionAST *ast) override;
    /// Visit the BlockAST and print it in a readable form
    /// @param ast The ast to visit
    void visitBlockAST(const BlockAST *ast) override;
    /// Visit the TypeAST and print it in a readable form
    /// @param ast The ast to visit
    void visitTypeAST(const TypeAST *ast) override;
    /// Visit the ParamAST and print it in a readable form
    /// @param ast The ast to visit
    void visitParamAST(const ParamAST *ast) override;
    /// Visit the ParamsAST and print it in a readable form
    /// @param ast The ast to visit
    void visitParamsAST(const ParamsAST *ast) override;
    /// Visit the VarStmtAST and print it in a readable form
    /// @param ast The ast to visit
    void visitVarStmtAST(const VarStmtAST *ast) override;
    /// Visit the AssignAST and print it in a readable form
    /// @param ast The ast to visit
    void visitAssignAST(const AssignAST *ast) override;
    /// Visit the VariableRefAST and print it in a readable form
    /// @param ast The ast to visit
    void visitVariableRefAST(const VariableRefAST *ast) override;
    /// Visit the ReturnStmtAST and print it in a readable form
    /// @param ast The ast to visit
    void visitReturnStmtAST(const ReturnStmtAST *ast) override;
    /// Visit the ArgAST and print it in a readable form
    /// @param ast The ast to visit
    void visitArgAST(const ArgAST *ast) override;
    /// Visit the ArgsAST and print it in a readable form
    /// @param ast The ast to visit
    void visitArgsAST(const ArgsAST *ast) override;
    /// Visit the CallAST and print it in a readable form
    /// @param ast The ast to visit
    void visitCallAST(const CallAST *ast) override;
    // GENPRINTVISITMETHOD END

private:
    /// Current indent value
    int indent;
    /// Whether or not to print the indent at this current character in the print() method
    bool pindent;
    /// Print a string to std::cout with indents
    /// @param str The string to print
    void print(std::string &str);
    /// Helper overload to print() that accepts rvalue references
    /// @param str The string to print
    void print(std::string &&str);
};

/// A visitor that does nothing for other partial visitors to extend
class BlankVisitor : public Visitor
{
public:
    // GENBLANKVISITMETHOD START
    /// Visit the BinaryAST and do nothing
    /// @param ast The ast to visit
    void visitBinaryAST(const BinaryAST *ast) override;
    /// Visit the TernaryOpAST and do nothing
    /// @param ast The ast to visit
    void visitTernaryOpAST(const TernaryOpAST *ast) override;
    /// Visit the UnaryAST and do nothing
    /// @param ast The ast to visit
    void visitUnaryAST(const UnaryAST *ast) override;
    /// Visit the PrimaryAST and do nothing
    /// @param ast The ast to visit
    void visitPrimaryAST(const PrimaryAST *ast) override;
    /// Visit the ExprStmtAST and do nothing
    /// @param ast The ast to visit
    void visitExprStmtAST(const ExprStmtAST *ast) override;
    /// Visit the ProgramAST and do nothing
    /// @param ast The ast to visit
    void visitProgramAST(const ProgramAST *ast) override;
    /// Visit the FunctionAST and do nothing
    /// @param ast The ast to visit
    void visitFunctionAST(const FunctionAST *ast) override;
    /// Visit the BlockAST and do nothing
    /// @param ast The ast to visit
    void visitBlockAST(const BlockAST *ast) override;
    /// Visit the TypeAST and do nothing
    /// @param ast The ast to visit
    void visitTypeAST(const TypeAST *ast) override;
    /// Visit the ParamAST and do nothing
    /// @param ast The ast to visit
    void visitParamAST(const ParamAST *ast) override;
    /// Visit the ParamsAST and do nothing
    /// @param ast The ast to visit
    void visitParamsAST(const ParamsAST *ast) override;
    /// Visit the VarStmtAST and do nothing
    /// @param ast The ast to visit
    void visitVarStmtAST(const VarStmtAST *ast) override;
    /// Visit the AssignAST and do nothing
    /// @param ast The ast to visit
    void visitAssignAST(const AssignAST *ast) override;
    /// Visit the VariableRefAST and do nothing
    /// @param ast The ast to visit
    void visitVariableRefAST(const VariableRefAST *ast) override;
    /// Visit the ReturnStmtAST and do nothing
    /// @param ast The ast to visit
    void visitReturnStmtAST(const ReturnStmtAST *ast) override;
    /// Visit the ArgAST and do nothing
    /// @param ast The ast to visit
    void visitArgAST(const ArgAST *ast) override;
    /// Visit the ArgsAST and do nothing
    /// @param ast The ast to visit
    void visitArgsAST(const ArgsAST *ast) override;
    /// Visit the CallAST and do nothing
    /// @param ast The ast to visit
    void visitCallAST(const CallAST *ast) override;
    // GENBLANKVISITMETHOD END
};

