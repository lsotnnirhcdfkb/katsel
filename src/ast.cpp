
#include "ast.h"

BinaryAST::BinaryAST(Token op, std::unique_ptr<AST> last, std::unique_ptr<AST> rast): op(op), last(std::move(last)), rast(std::move(rast)) {}
void BinaryAST::accept(Visitor *v) { v->visitBinaryAST(this); }

TernaryOpAST::TernaryOpAST(std::unique_ptr<AST> conditional, std::unique_ptr<AST> trueast, std::unique_ptr<AST> falseast): conditional(std::move(conditional)), trueast(std::move(trueast)), falseast(std::move(falseast)) {}
void TernaryOpAST::accept(Visitor *v) { v->visitTernaryOpAST(this); }

UnaryAST::UnaryAST(Token op, std::unique_ptr<AST> ast): op(op), ast(std::move(ast)) {}
void UnaryAST::accept(Visitor *v) { v->visitUnaryAST(this); }

PrimaryAST::PrimaryAST(Token value): value(value) {}
void PrimaryAST::accept(Visitor *v) { v->visitPrimaryAST(this); }

ExprStmtAST::ExprStmtAST(std::unique_ptr<AST> ast): ast(std::move(ast)) {}
void ExprStmtAST::accept(Visitor *v) { v->visitExprStmtAST(this); }

ProgramAST::ProgramAST(std::vector<std::unique_ptr<AST>> &asts)
{
    this->asts.reserve(asts.size());
    for (std::unique_ptr<AST> &ast : asts)
    {
        this->asts.push_back(std::move(ast));
    }
}
void ProgramAST::accept(Visitor *v) { v->visitProgramAST(this); }

FunctionAST::FunctionAST(std::unique_ptr<AST> type, Token name, std::unique_ptr<AST> params, std::unique_ptr<AST> body): type(std::move(type)), name(name), params(std::move(params)), body(std::move(body)) {}
void FunctionAST::accept(Visitor *v) { v->visitFunctionAST(this); }

BlockAST::BlockAST(std::vector<std::unique_ptr<AST>> &stmts)
{
    this->stmts.reserve(stmts.size());
    for (std::unique_ptr<AST> &ast : stmts)
    {
        this->stmts.push_back(std::move(ast));
    }
}
void BlockAST::accept(Visitor *v) { v->visitBlockAST(this); }

TypeAST::TypeAST(Token type): type(type) {}
void TypeAST::accept(Visitor *v) { v->visitTypeAST(this); }

ParamAST::ParamAST(std::unique_ptr<AST> type, Token paramname): type(std::move(type)), paramname(paramname) {}
void ParamAST::accept(Visitor *v) { v->visitParamAST(this); }

ParamsAST::ParamsAST(std::vector<std::unique_ptr<AST>> &params)
{
    this->params.reserve(params.size());
    for (std::unique_ptr<AST> &ast : params)
    {
        this->params.push_back(std::move(ast));
    }
}
void ParamsAST::accept(Visitor *v) { v->visitParamsAST(this); }

VarStmtAST::VarStmtAST(std::unique_ptr<AST> type, Token name, std::unique_ptr<AST> expression): type(std::move(type)), name(name), expression(std::move(expression)) {}
void VarStmtAST::accept(Visitor *v) { v->visitVarStmtAST(this); }

AssignAST::AssignAST(std::unique_ptr<AST> lhs, std::unique_ptr<AST> rhs, Token equalSign): lhs(std::move(lhs)), rhs(std::move(rhs)), equalSign(equalSign) {}
void AssignAST::accept(Visitor *v) { v->visitAssignAST(this); }

VariableRefAST::VariableRefAST(Token var): var(var) {}
void VariableRefAST::accept(Visitor *v) { v->visitVariableRefAST(this); }

ReturnStmtAST::ReturnStmtAST(std::unique_ptr<AST> expr): expr(std::move(expr)) {}
void ReturnStmtAST::accept(Visitor *v) { v->visitReturnStmtAST(this); }

ArgAST::ArgAST(std::unique_ptr<AST> expr): expr(std::move(expr)) {}
void ArgAST::accept(Visitor *v) { v->visitArgAST(this); }

ArgsAST::ArgsAST(std::vector<std::unique_ptr<AST>> &args)
{
    this->args.reserve(args.size());
    for (std::unique_ptr<AST> &ast : args)
    {
        this->args.push_back(std::move(ast));
    }
}
void ArgsAST::accept(Visitor *v) { v->visitArgsAST(this); }

CallAST::CallAST(std::unique_ptr<AST> varrefast, std::unique_ptr<AST> arglistast): varrefast(std::move(varrefast)), arglistast(std::move(arglistast)) {}
void CallAST::accept(Visitor *v) { v->visitCallAST(this); }

