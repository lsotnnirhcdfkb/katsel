/// @file ast.cpp
/// AST method declarations

#include "ast.h"

ASTs::BinaryAST::BinaryAST(Token op, std::unique_ptr<ASTs::AST> last, std::unique_ptr<ASTs::AST> rast): op(op), last(std::move(last)), rast(std::move(rast)) {}

ASTs::TernaryOpAST::TernaryOpAST(std::unique_ptr<ASTs::AST> conditional, std::unique_ptr<ASTs::AST> trueast, std::unique_ptr<ASTs::AST> falseast): conditional(std::move(conditional)), trueast(std::move(trueast)), falseast(std::move(falseast)) {}

ASTs::UnaryAST::UnaryAST(Token op, std::unique_ptr<ASTs::AST> ast): op(op), ast(std::move(ast)) {}

ASTs::PrimaryAST::PrimaryAST(Token value): value(value) {}

ASTs::ExprStmtAST::ExprStmtAST(std::unique_ptr<ASTs::AST> ast): ast(std::move(ast)) {}

ASTs::ProgramAST::ProgramAST(std::vector<std::unique_ptr<ASTs::AST>> &asts)
{
    this->asts.reserve(asts.size());
    for (std::unique_ptr<ASTs::AST> &ast : asts)
    {
        this->asts.push_back(std::move(ast));
    }
}

ASTs::FunctionAST::FunctionAST(std::unique_ptr<ASTs::AST> type, Token name, std::unique_ptr<ASTs::AST> params, std::unique_ptr<ASTs::AST> body): type(std::move(type)), name(name), params(std::move(params)), body(std::move(body)) {}

ASTs::BlockAST::BlockAST(std::vector<std::unique_ptr<ASTs::AST>> &stmts)
{
    this->stmts.reserve(stmts.size());
    for (std::unique_ptr<ASTs::AST> &ast : stmts)
    {
        this->stmts.push_back(std::move(ast));
    }
}

ASTs::TypeAST::TypeAST(Token type): type(type) {}

ASTs::ParamAST::ParamAST(std::unique_ptr<ASTs::AST> type, Token paramname): type(std::move(type)), paramname(paramname) {}

ASTs::ParamsAST::ParamsAST(std::vector<std::unique_ptr<ASTs::AST>> &params)
{
    this->params.reserve(params.size());
    for (std::unique_ptr<ASTs::AST> &ast : params)
    {
        this->params.push_back(std::move(ast));
    }
}

ASTs::VarStmtAST::VarStmtAST(std::unique_ptr<ASTs::AST> type, Token name, std::unique_ptr<ASTs::AST> expression): type(std::move(type)), name(name), expression(std::move(expression)) {}

ASTs::AssignAST::AssignAST(std::unique_ptr<ASTs::AST> lhs, std::unique_ptr<ASTs::AST> rhs, Token equalSign): lhs(std::move(lhs)), rhs(std::move(rhs)), equalSign(equalSign) {}

ASTs::VariableRefAST::VariableRefAST(Token var): var(var) {}

ASTs::LValueAST::LValueAST(std::unique_ptr<ASTs::AST> expr): expr(std::move(expr)) {}

ASTs::ReturnStmtAST::ReturnStmtAST(std::unique_ptr<ASTs::AST> expr): expr(std::move(expr)) {}

ASTs::ArgAST::ArgAST(std::unique_ptr<ASTs::AST> expr): expr(std::move(expr)) {}

ASTs::ArgsAST::ArgsAST(std::vector<std::unique_ptr<ASTs::AST>> &args)
{
    this->args.reserve(args.size());
    for (std::unique_ptr<ASTs::AST> &ast : args)
    {
        this->args.push_back(std::move(ast));
    }
}

ASTs::CallAST::CallAST(std::unique_ptr<ASTs::AST> varrefast, std::unique_ptr<ASTs::AST> arglistast, Token oparn): varrefast(std::move(varrefast)), arglistast(std::move(arglistast)), oparn(oparn) {}

