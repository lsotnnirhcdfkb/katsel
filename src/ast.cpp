/// @file ast.cpp
/// AST method declarations

#include "ast.h"

ASTs::BinaryAST::BinaryAST(Token op, std::unique_ptr<ASTs::AST> last, std::unique_ptr<ASTs::AST> rast): op(op), last(std::move(last)), rast(std::move(rast)) {}
void ASTs::BinaryAST::accept(Visitor *v) { v->visitBinaryAST(this); }

ASTs::TernaryOpAST::TernaryOpAST(std::unique_ptr<ASTs::AST> conditional, std::unique_ptr<ASTs::AST> trueast, std::unique_ptr<ASTs::AST> falseast): conditional(std::move(conditional)), trueast(std::move(trueast)), falseast(std::move(falseast)) {}
void ASTs::TernaryOpAST::accept(Visitor *v) { v->visitTernaryOpAST(this); }

ASTs::UnaryAST::UnaryAST(Token op, std::unique_ptr<ASTs::AST> ast): op(op), ast(std::move(ast)) {}
void ASTs::UnaryAST::accept(Visitor *v) { v->visitUnaryAST(this); }

ASTs::PrimaryAST::PrimaryAST(Token value): value(value) {}
void ASTs::PrimaryAST::accept(Visitor *v) { v->visitPrimaryAST(this); }

ASTs::ExprStmtAST::ExprStmtAST(std::unique_ptr<ASTs::AST> ast): ast(std::move(ast)) {}
void ASTs::ExprStmtAST::accept(Visitor *v) { v->visitExprStmtAST(this); }

ASTs::ProgramAST::ProgramAST(std::vector<std::unique_ptr<ASTs::AST>> &asts)
{
    this->asts.reserve(asts.size());
    for (std::unique_ptr<ASTs::AST> &ast : asts)
    {
        this->asts.push_back(std::move(ast));
    }
}
void ASTs::ProgramAST::accept(Visitor *v) { v->visitProgramAST(this); }

ASTs::FunctionAST::FunctionAST(std::unique_ptr<ASTs::AST> type, Token name, std::unique_ptr<ASTs::AST> params, std::unique_ptr<ASTs::AST> body): type(std::move(type)), name(name), params(std::move(params)), body(std::move(body)) {}
void ASTs::FunctionAST::accept(Visitor *v) { v->visitFunctionAST(this); }

ASTs::BlockAST::BlockAST(std::vector<std::unique_ptr<ASTs::AST>> &stmts)
{
    this->stmts.reserve(stmts.size());
    for (std::unique_ptr<ASTs::AST> &ast : stmts)
    {
        this->stmts.push_back(std::move(ast));
    }
}
void ASTs::BlockAST::accept(Visitor *v) { v->visitBlockAST(this); }

ASTs::TypeAST::TypeAST(Token type): type(type) {}
void ASTs::TypeAST::accept(Visitor *v) { v->visitTypeAST(this); }

ASTs::ParamAST::ParamAST(std::unique_ptr<ASTs::AST> type, Token paramname): type(std::move(type)), paramname(paramname) {}
void ASTs::ParamAST::accept(Visitor *v) { v->visitParamAST(this); }

ASTs::ParamsAST::ParamsAST(std::vector<std::unique_ptr<ASTs::AST>> &params)
{
    this->params.reserve(params.size());
    for (std::unique_ptr<ASTs::AST> &ast : params)
    {
        this->params.push_back(std::move(ast));
    }
}
void ASTs::ParamsAST::accept(Visitor *v) { v->visitParamsAST(this); }

ASTs::VarStmtAST::VarStmtAST(std::unique_ptr<ASTs::AST> type, Token name, std::unique_ptr<ASTs::AST> expression): type(std::move(type)), name(name), expression(std::move(expression)) {}
void ASTs::VarStmtAST::accept(Visitor *v) { v->visitVarStmtAST(this); }

ASTs::AssignAST::AssignAST(std::unique_ptr<ASTs::AST> lhs, std::unique_ptr<ASTs::AST> rhs, Token equalSign): lhs(std::move(lhs)), rhs(std::move(rhs)), equalSign(equalSign) {}
void ASTs::AssignAST::accept(Visitor *v) { v->visitAssignAST(this); }

ASTs::VariableRefAST::VariableRefAST(Token var): var(var) {}
void ASTs::VariableRefAST::accept(Visitor *v) { v->visitVariableRefAST(this); }

ASTs::ReturnStmtAST::ReturnStmtAST(std::unique_ptr<ASTs::AST> expr): expr(std::move(expr)) {}
void ASTs::ReturnStmtAST::accept(Visitor *v) { v->visitReturnStmtAST(this); }

ASTs::ArgAST::ArgAST(std::unique_ptr<ASTs::AST> expr): expr(std::move(expr)) {}
void ASTs::ArgAST::accept(Visitor *v) { v->visitArgAST(this); }

ASTs::ArgsAST::ArgsAST(std::vector<std::unique_ptr<ASTs::AST>> &args)
{
    this->args.reserve(args.size());
    for (std::unique_ptr<ASTs::AST> &ast : args)
    {
        this->args.push_back(std::move(ast));
    }
}
void ASTs::ArgsAST::accept(Visitor *v) { v->visitArgsAST(this); }

ASTs::CallAST::CallAST(std::unique_ptr<ASTs::AST> varrefast, std::unique_ptr<ASTs::AST> arglistast, Token oparn): varrefast(std::move(varrefast)), arglistast(std::move(arglistast)), oparn(oparn) {}
void ASTs::CallAST::accept(Visitor *v) { v->visitCallAST(this); }

