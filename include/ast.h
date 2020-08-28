#pragma once

#include <vector>
#include <string>
#include <iostream>
#include <memory>
#include "token.h"
#include "visitor.h"

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
    BinaryAST(Token op, std::unique_ptr<AST> last, std::unique_ptr<AST> rast);
    /// The accept method that calls the correct visitor method on this AST
    void accept(Visitor *v) override;

    /// The operator
    Token op;
    /// The left operand
    std::unique_ptr<AST> last;
    /// The right operator
    std::unique_ptr<AST> rast;
};

/// An AST for the ternary operator (?:)
class TernaryOpAST : public AST
{
public:
    /// The constructor for this class
    /// @param conditional 
    /// @param trueast 
    /// @param falseast 
    TernaryOpAST(std::unique_ptr<AST> conditional, std::unique_ptr<AST> trueast, std::unique_ptr<AST> falseast);
    /// The accept method that calls the correct visitor method on this AST
    void accept(Visitor *v) override;

    /// 
    std::unique_ptr<AST> conditional;
    /// 
    std::unique_ptr<AST> trueast;
    /// 
    std::unique_ptr<AST> falseast;
};

/// An AST for unary operators
class UnaryAST : public AST
{
public:
    /// The constructor for this class
    /// @param op 
    /// @param ast 
    UnaryAST(Token op, std::unique_ptr<AST> ast);
    /// The accept method that calls the correct visitor method on this AST
    void accept(Visitor *v) override;

    /// 
    Token op;
    /// 
    std::unique_ptr<AST> ast;
};

/// An AST for primary tokens (literals etc.)
class PrimaryAST : public AST
{
public:
    /// The constructor for this class
    /// @param value 
    PrimaryAST(Token value);
    /// The accept method that calls the correct visitor method on this AST
    void accept(Visitor *v) override;

    /// 
    Token value;
};

/// An AST for an expression statement
class ExprStmtAST : public AST
{
public:
    /// The constructor for this class
    /// @param ast 
    ExprStmtAST(std::unique_ptr<AST> ast);
    /// The accept method that calls the correct visitor method on this AST
    void accept(Visitor *v) override;

    /// 
    std::unique_ptr<AST> ast;
};

/// An AST representing an entire program
class ProgramAST : public AST
{
public:
    /// The constructor for this class
    /// @param asts 
    ProgramAST(std::vector<std::unique_ptr<AST>> &asts);
    /// The accept method that calls the correct visitor method on this AST
    void accept(Visitor *v) override;

    /// 
    std::vector<std::unique_ptr<AST>> asts;
};

/// An AST representing a function declaration or definition
class FunctionAST : public AST
{
public:
    /// The constructor for this class
    /// @param type 
    /// @param name 
    /// @param params 
    /// @param body 
    FunctionAST(std::unique_ptr<AST> type, Token name, std::unique_ptr<AST> params, std::unique_ptr<AST> body);
    /// The accept method that calls the correct visitor method on this AST
    void accept(Visitor *v) override;

    /// 
    std::unique_ptr<AST> type;
    /// 
    Token name;
    /// 
    std::unique_ptr<AST> params;
    /// 
    std::unique_ptr<AST> body;
};

/// An AST representing a code block
class BlockAST : public AST
{
public:
    /// The constructor for this class
    /// @param stmts 
    BlockAST(std::vector<std::unique_ptr<AST>> &stmts);
    /// The accept method that calls the correct visitor method on this AST
    void accept(Visitor *v) override;

    /// 
    std::vector<std::unique_ptr<AST>> stmts;
};

/// An AST for a type
class TypeAST : public AST
{
public:
    /// The constructor for this class
    /// @param type 
    TypeAST(Token type);
    /// The accept method that calls the correct visitor method on this AST
    void accept(Visitor *v) override;

    /// 
    Token type;
};

/// An AST representing a parameter
class ParamAST : public AST
{
public:
    /// The constructor for this class
    /// @param type 
    /// @param paramname 
    ParamAST(std::unique_ptr<AST> type, Token paramname);
    /// The accept method that calls the correct visitor method on this AST
    void accept(Visitor *v) override;

    /// 
    std::unique_ptr<AST> type;
    /// 
    Token paramname;
};

/// An AST representing a parameter list
class ParamsAST : public AST
{
public:
    /// The constructor for this class
    /// @param params 
    ParamsAST(std::vector<std::unique_ptr<AST>> &params);
    /// The accept method that calls the correct visitor method on this AST
    void accept(Visitor *v) override;

    /// 
    std::vector<std::unique_ptr<AST>> params;
};

/// An AST representing a variable declaration statement
class VarStmtAST : public AST
{
public:
    /// The constructor for this class
    /// @param type 
    /// @param name 
    /// @param expression 
    VarStmtAST(std::unique_ptr<AST> type, Token name, std::unique_ptr<AST> expression);
    /// The accept method that calls the correct visitor method on this AST
    void accept(Visitor *v) override;

    /// 
    std::unique_ptr<AST> type;
    /// 
    Token name;
    /// 
    std::unique_ptr<AST> expression;
};

/// An AST representing an assignment expression
class AssignAST : public AST
{
public:
    /// The constructor for this class
    /// @param lhs 
    /// @param rhs 
    /// @param equalSign 
    AssignAST(std::unique_ptr<AST> lhs, std::unique_ptr<AST> rhs, Token equalSign);
    /// The accept method that calls the correct visitor method on this AST
    void accept(Visitor *v) override;

    /// 
    std::unique_ptr<AST> lhs;
    /// 
    std::unique_ptr<AST> rhs;
    /// 
    Token equalSign;
};

/// An AST for a variable reference
class VariableRefAST : public AST
{
public:
    /// The constructor for this class
    /// @param var 
    VariableRefAST(Token var);
    /// The accept method that calls the correct visitor method on this AST
    void accept(Visitor *v) override;

    /// 
    Token var;
};

/// An AST representing a return statement
class ReturnStmtAST : public AST
{
public:
    /// The constructor for this class
    /// @param expr 
    ReturnStmtAST(std::unique_ptr<AST> expr);
    /// The accept method that calls the correct visitor method on this AST
    void accept(Visitor *v) override;

    /// 
    std::unique_ptr<AST> expr;
};

/// An AST representing an arguemnt passed into a function call
class ArgAST : public AST
{
public:
    /// The constructor for this class
    /// @param expr 
    ArgAST(std::unique_ptr<AST> expr);
    /// The accept method that calls the correct visitor method on this AST
    void accept(Visitor *v) override;

    /// 
    std::unique_ptr<AST> expr;
};

/// An AST representing arguments passed into a function call
class ArgsAST : public AST
{
public:
    /// The constructor for this class
    /// @param args 
    ArgsAST(std::vector<std::unique_ptr<AST>> &args);
    /// The accept method that calls the correct visitor method on this AST
    void accept(Visitor *v) override;

    /// 
    std::vector<std::unique_ptr<AST>> args;
};

/// An AST representing a function call
class CallAST : public AST
{
public:
    /// The constructor for this class
    /// @param varrefast 
    /// @param arglistast 
    /// @param oparn 
    CallAST(std::unique_ptr<AST> varrefast, std::unique_ptr<AST> arglistast, Token oparn);
    /// The accept method that calls the correct visitor method on this AST
    void accept(Visitor *v) override;

    /// 
    std::unique_ptr<AST> varrefast;
    /// 
    std::unique_ptr<AST> arglistast;
    /// 
    Token oparn;
};

// GENASTHEADER END
