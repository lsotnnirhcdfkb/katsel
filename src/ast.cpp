#include "ast.h"

BinaryAST::BinaryAST(Token op, std::unique_ptr<AST> last, std::unique_ptr<AST> rast): op(op), last(std::move(last)), rast(std::move(rast)) {}
TernaryOpAST::TernaryOpAST(std::unique_ptr<AST> conditional, std::unique_ptr<AST> trueast, std::unique_ptr<AST> falseast): conditional(std::move(conditional)), trueast(std::move(trueast)), falseast(std::move(falseast)) {}
UnaryAST::UnaryAST(Token op, std::unique_ptr<AST> ast): op(op), ast(std::move(ast)) {}
PrimaryAST::PrimaryAST(Token value): value(value) {}
ExprStmtAST::ExprStmtAST(std::unique_ptr<AST> ast): ast(std::move(ast)) {}
FunctionAST::FunctionAST(std::unique_ptr<AST> type, Token name, std::unique_ptr<AST> params, std::unique_ptr<AST> body): type(std::move(type)), name(name), params(std::move(params)), body(std::move(body)) {}
TypeAST::TypeAST(Token type): type(type) {}
ParamAST::ParamAST(std::unique_ptr<AST> type, Token paramname): type(std::move(type)), paramname(paramname) {}
VarStmtAST::VarStmtAST(std::unique_ptr<AST> type, Token name, std::unique_ptr<AST> expression) : type(std::move(type)), name(name), expression(std::move(expression)) {}
AssignAST::AssignAST(std::unique_ptr<AST> lhs, std::unique_ptr<AST> rhs, Token equalSign) : lhs(std::move(lhs)), rhs(std::move(rhs)), equalSign(equalSign) {}
VariableRefAST::VariableRefAST(Token var): var(var) {}
ReturnStmtAST::ReturnStmtAST(std::unique_ptr<AST> expr): expr(std::move(expr)) {}
ArgAST::ArgAST(std::unique_ptr<AST> expr): expr(std::move(expr)) {}
CallAST::CallAST(std::unique_ptr<AST> varrefast, std::unique_ptr<AST> arglistast): varrefast(std::move(varrefast)), arglistast(std::move(arglistast)) {}

ProgramAST::ProgramAST(std::vector<std::unique_ptr<AST>> &asts) 
{
    this->asts.reserve(asts.size());
    for (std::unique_ptr<AST> &ast : asts)
    {
        this->asts.push_back(std::move(ast));
    }
}
BlockAST::BlockAST(std::vector<std::unique_ptr<AST>> &stmts)
{
    this->stmts.reserve(stmts.size());
    for (std::unique_ptr<AST> &stmt : stmts)
    {
        this->stmts.push_back(std::move(stmt));
    }
}
ParamsAST::ParamsAST(std::vector<std::unique_ptr<AST>> &params)
{
    this->params.reserve(params.size());
    for (std::unique_ptr<AST> &param : params)
    {
        this->params.push_back(std::move(param));
    }
}
ArgsAST::ArgsAST(std::vector<std::unique_ptr<AST>> &args)
{
    this->args.reserve(args.size());
    for (std::unique_ptr<AST> &arg : args)
    {
        this->args.push_back(std::move(arg));
    }
}

void BinaryAST::accept(Visitor *v) { v->visitBinaryAST(this); }
void TernaryOpAST::accept(Visitor *v) { v->visitTernaryOpAST(this); }
void UnaryAST::accept(Visitor *v) { v->visitUnaryAST(this); }
void PrimaryAST::accept(Visitor *v) { v->visitPrimaryAST(this); }
void ExprStmtAST::accept(Visitor *v) { v->visitExprStmtAST(this); }
void ProgramAST::accept(Visitor *v) { v->visitProgramAST(this); }
void FunctionAST::accept(Visitor *v) { v->visitFunctionAST(this); }
void BlockAST::accept(Visitor *v) { v->visitBlockAST(this); }
void TypeAST::accept(Visitor *v) { v->visitTypeAST(this); }
void ParamAST::accept(Visitor *v) { v->visitParamAST(this); }
void ParamsAST::accept(Visitor *v) { v->visitParamsAST(this); }
void VarStmtAST::accept(Visitor *v) { v->visitVarStmtAST(this); }
void AssignAST::accept(Visitor *v) { v->visitAssignAST(this); }
void VariableRefAST::accept(Visitor *v) { v->visitVariableRefAST(this); }
void ReturnStmtAST::accept(Visitor *v) { v->visitReturnStmtAST(this); }
void ArgAST::accept(Visitor *v) { }
void ArgsAST::accept(Visitor *v) { }
void CallAST::accept(Visitor *v) { }
