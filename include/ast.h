/// @file ast.h
/// Declarations for all the AST classes

#pragma once

#include <vector>
#include <string>
#include <iostream>
#include <memory>
#include "token.h"
#include "visitor.h"

/// A namespace to hold all the AST classes in
namespace ASTs
{
    // GENASTHEADER START
    /// A base AST class
    class AST
    {
    public:
        /// The virtual constructor
        virtual ~AST() {}
    
        /// A pure virtual accept method that each AST class is supposed to implement to call the right visitor method
        virtual void accept(Visitor *v) = 0;
    };

    /// An AST for binary operators
    class BinaryAST : public AST
    {
    public:
        /// The constructor for this class
        /// @param op The operator
        /// @param last The left operand
        /// @param rast The right operator
        BinaryAST(Token op, std::unique_ptr<ASTs::AST> last, std::unique_ptr<ASTs::AST> rast);
        /// The accept method that calls the correct visitor method on this AST
        void accept(Visitor *v) override;
    
        /// The operator
        Token op;
        /// The left operand
        std::unique_ptr<ASTs::AST> last;
        /// The right operator
        std::unique_ptr<ASTs::AST> rast;
    };

    /// An AST for the ternary operator (?:)
    class TernaryOpAST : public AST
    {
    public:
        /// The constructor for this class
        /// @param conditional The conditional expression
        /// @param trueast The expression that this evaluates to if the codnditional is true
        /// @param falseast The expression that this evaluates to if the codnditional is false
        TernaryOpAST(std::unique_ptr<ASTs::AST> conditional, std::unique_ptr<ASTs::AST> trueast, std::unique_ptr<ASTs::AST> falseast);
        /// The accept method that calls the correct visitor method on this AST
        void accept(Visitor *v) override;
    
        /// The conditional expression
        std::unique_ptr<ASTs::AST> conditional;
        /// The expression that this evaluates to if the codnditional is true
        std::unique_ptr<ASTs::AST> trueast;
        /// The expression that this evaluates to if the codnditional is false
        std::unique_ptr<ASTs::AST> falseast;
    };

    /// An AST for unary operators
    class UnaryAST : public AST
    {
    public:
        /// The constructor for this class
        /// @param op The operator
        /// @param ast The operand
        UnaryAST(Token op, std::unique_ptr<ASTs::AST> ast);
        /// The accept method that calls the correct visitor method on this AST
        void accept(Visitor *v) override;
    
        /// The operator
        Token op;
        /// The operand
        std::unique_ptr<ASTs::AST> ast;
    };

    /// An AST for primary tokens (literals etc.)
    class PrimaryAST : public AST
    {
    public:
        /// The constructor for this class
        /// @param value The value
        PrimaryAST(Token value);
        /// The accept method that calls the correct visitor method on this AST
        void accept(Visitor *v) override;
    
        /// The value
        Token value;
    };

    /// An AST for an expression statement
    class ExprStmtAST : public AST
    {
    public:
        /// The constructor for this class
        /// @param ast The expression of this statement
        ExprStmtAST(std::unique_ptr<ASTs::AST> ast);
        /// The accept method that calls the correct visitor method on this AST
        void accept(Visitor *v) override;
    
        /// The expression of this statement
        std::unique_ptr<ASTs::AST> ast;
    };

    /// An AST representing an entire program
    class ProgramAST : public AST
    {
    public:
        /// The constructor for this class
        /// @param asts The asts that this program has
        ProgramAST(std::vector<std::unique_ptr<ASTs::AST>> &asts);
        /// The accept method that calls the correct visitor method on this AST
        void accept(Visitor *v) override;
    
        /// The asts that this program has
        std::vector<std::unique_ptr<ASTs::AST>> asts;
    };

    /// An AST representing a function declaration or definition
    class FunctionAST : public AST
    {
    public:
        /// The constructor for this class
        /// @param type The return type of the function
        /// @param name The name of the function
        /// @param params The parameters of the function
        /// @param body The body of the function
        FunctionAST(std::unique_ptr<ASTs::AST> type, Token name, std::unique_ptr<ASTs::AST> params, std::unique_ptr<ASTs::AST> body);
        /// The accept method that calls the correct visitor method on this AST
        void accept(Visitor *v) override;
    
        /// The return type of the function
        std::unique_ptr<ASTs::AST> type;
        /// The name of the function
        Token name;
        /// The parameters of the function
        std::unique_ptr<ASTs::AST> params;
        /// The body of the function
        std::unique_ptr<ASTs::AST> body;
    };

    /// An AST representing a code block
    class BlockAST : public AST
    {
    public:
        /// The constructor for this class
        /// @param stmts The statements in the block
        BlockAST(std::vector<std::unique_ptr<ASTs::AST>> &stmts);
        /// The accept method that calls the correct visitor method on this AST
        void accept(Visitor *v) override;
    
        /// The statements in the block
        std::vector<std::unique_ptr<ASTs::AST>> stmts;
    };

    /// An AST for a type
    class TypeAST : public AST
    {
    public:
        /// The constructor for this class
        /// @param type The type token
        TypeAST(Token type);
        /// The accept method that calls the correct visitor method on this AST
        void accept(Visitor *v) override;
    
        /// The type token
        Token type;
    };

    /// An AST representing a parameter
    class ParamAST : public AST
    {
    public:
        /// The constructor for this class
        /// @param type The type of the parameter
        /// @param paramname The name of the parameter
        ParamAST(std::unique_ptr<ASTs::AST> type, Token paramname);
        /// The accept method that calls the correct visitor method on this AST
        void accept(Visitor *v) override;
    
        /// The type of the parameter
        std::unique_ptr<ASTs::AST> type;
        /// The name of the parameter
        Token paramname;
    };

    /// An AST representing a parameter list
    class ParamsAST : public AST
    {
    public:
        /// The constructor for this class
        /// @param params A vector of parameters
        ParamsAST(std::vector<std::unique_ptr<ASTs::AST>> &params);
        /// The accept method that calls the correct visitor method on this AST
        void accept(Visitor *v) override;
    
        /// A vector of parameters
        std::vector<std::unique_ptr<ASTs::AST>> params;
    };

    /// An AST representing a variable declaration statement
    class VarStmtAST : public AST
    {
    public:
        /// The constructor for this class
        /// @param type The type of the variable
        /// @param name The name of the variable
        /// @param expression The expression being assigned to the variable
        VarStmtAST(std::unique_ptr<ASTs::AST> type, Token name, std::unique_ptr<ASTs::AST> expression);
        /// The accept method that calls the correct visitor method on this AST
        void accept(Visitor *v) override;
    
        /// The type of the variable
        std::unique_ptr<ASTs::AST> type;
        /// The name of the variable
        Token name;
        /// The expression being assigned to the variable
        std::unique_ptr<ASTs::AST> expression;
    };

    /// An AST representing an assignment expression
    class AssignAST : public AST
    {
    public:
        /// The constructor for this class
        /// @param lhs The thing to assign to
        /// @param rhs The expression to assign
        /// @param equalSign A token to error at in case there is an error
        AssignAST(std::unique_ptr<ASTs::AST> lhs, std::unique_ptr<ASTs::AST> rhs, Token equalSign);
        /// The accept method that calls the correct visitor method on this AST
        void accept(Visitor *v) override;
    
        /// The thing to assign to
        std::unique_ptr<ASTs::AST> lhs;
        /// The expression to assign
        std::unique_ptr<ASTs::AST> rhs;
        /// A token to error at in case there is an error
        Token equalSign;
    };

    /// An AST for a variable reference
    class VariableRefAST : public AST
    {
    public:
        /// The constructor for this class
        /// @param var The variable being referenced
        VariableRefAST(Token var);
        /// The accept method that calls the correct visitor method on this AST
        void accept(Visitor *v) override;
    
        /// The variable being referenced
        Token var;
    };

    /// An AST for any expression that is guaranteed to evaluatae to an lvalue
    class LValueAST : public AST
    {
    public:
        /// The constructor for this class
        /// @param expr The expression that evaluates to an lvalue
        LValueAST(std::unique_ptr<ASTs::AST> expr);
        /// The accept method that calls the correct visitor method on this AST
        void accept(Visitor *v) override;
    
        /// The expression that evaluates to an lvalue
        std::unique_ptr<ASTs::AST> expr;
    };

    /// An AST representing a return statement
    class ReturnStmtAST : public AST
    {
    public:
        /// The constructor for this class
        /// @param expr The expression to return
        ReturnStmtAST(std::unique_ptr<ASTs::AST> expr);
        /// The accept method that calls the correct visitor method on this AST
        void accept(Visitor *v) override;
    
        /// The expression to return
        std::unique_ptr<ASTs::AST> expr;
    };

    /// An AST representing an arguemnt passed into a function call
    class ArgAST : public AST
    {
    public:
        /// The constructor for this class
        /// @param expr The expression that the argument is
        ArgAST(std::unique_ptr<ASTs::AST> expr);
        /// The accept method that calls the correct visitor method on this AST
        void accept(Visitor *v) override;
    
        /// The expression that the argument is
        std::unique_ptr<ASTs::AST> expr;
    };

    /// An AST representing arguments passed into a function call
    class ArgsAST : public AST
    {
    public:
        /// The constructor for this class
        /// @param args A vector of arguments
        ArgsAST(std::vector<std::unique_ptr<ASTs::AST>> &args);
        /// The accept method that calls the correct visitor method on this AST
        void accept(Visitor *v) override;
    
        /// A vector of arguments
        std::vector<std::unique_ptr<ASTs::AST>> args;
    };

    /// An AST representing a function call
    class CallAST : public AST
    {
    public:
        /// The constructor for this class
        /// @param varrefast The variable reference that is being called
        /// @param arglistast The argument list for the function call
        /// @param oparn The opening parentheses to throw an error at
        CallAST(std::unique_ptr<ASTs::AST> varrefast, std::unique_ptr<ASTs::AST> arglistast, Token oparn);
        /// The accept method that calls the correct visitor method on this AST
        void accept(Visitor *v) override;
    
        /// The variable reference that is being called
        std::unique_ptr<ASTs::AST> varrefast;
        /// The argument list for the function call
        std::unique_ptr<ASTs::AST> arglistast;
        /// The opening parentheses to throw an error at
        Token oparn;
    };

    // GENASTHEADER END
}
