#include "ast.h"
ASTNS::Program::Program(std::vector<std::unique_ptr<Decl>> &decls)
{
    for (auto &p : decls) this->decls.push_back(std::move(p)); decls.clear();
}
ASTNS::BinaryExpr::BinaryExpr(std::unique_ptr<Expr> lhs, std::unique_ptr<Expr> rhs, Token op)
{
    this->lhs = std::move(lhs);
    this->rhs = std::move(rhs);
    this->op = op;
}
ASTNS::TernaryExpr::TernaryExpr(std::unique_ptr<Expr> condition, std::unique_ptr<Expr> trues, std::unique_ptr<Expr> falses)
{
    this->condition = std::move(condition);
    this->trues = std::move(trues);
    this->falses = std::move(falses);
}
ASTNS::UnaryExpr::UnaryExpr(std::unique_ptr<Expr> operand, Token op)
{
    this->operand = std::move(operand);
    this->op = op;
}
ASTNS::PrimaryExpr::PrimaryExpr(Token value)
{
    this->value = value;
}
ASTNS::AssignExpr::AssignExpr(std::unique_ptr<Expr> assignee, std::unique_ptr<Expr> value)
{
    this->assignee = std::move(assignee);
    this->value = std::move(value);
}
ASTNS::CallExpr::CallExpr(std::unique_ptr<Expr> func, std::unique_ptr<Arg> args)
{
    this->func = std::move(func);
    this->args = std::move(args);
}
ASTNS::LtoRVExpr::LtoRVExpr(std::unique_ptr<Expr> val)
{
    this->val = std::move(val);
}
ASTNS::BlockStmt::BlockStmt(std::vector<std::unique_ptr<Stmt>> &stmts)
{
    for (auto &p : stmts) this->stmts.push_back(std::move(p)); stmts.clear();
}
ASTNS::ExprStmt::ExprStmt(std::unique_ptr<Expr> expr)
{
    this->expr = std::move(expr);
}
ASTNS::ReturnStmt::ReturnStmt(std::unique_ptr<Expr> val)
{
    this->val = std::move(val);
}
ASTNS::VarStmt::VarStmt(std::unique_ptr<Type> type, Token name, std::unique_ptr<Expr> value)
{
    this->type = std::move(type);
    this->name = name;
    this->value = std::move(value);
}
ASTNS::VarRef::VarRef(Token var)
{
    this->var = var;
}
ASTNS::BaseType::BaseType(Token type)
{
    this->type = type;
}
ASTNS::FunctionDecl::FunctionDecl(std::unique_ptr<Type> rettype, Token name, std::unique_ptr<Param> params, std::unique_ptr<BlockStmt> block)
{
    this->rettype = std::move(rettype);
    this->name = name;
    this->params = std::move(params);
    this->block = std::move(block);
}
ASTNS::GlobalVarDecl::GlobalVarDecl(std::unique_ptr<Type> type, Token name, std::unique_ptr<Expr> value)
{
    this->type = std::move(type);
    this->name = name;
    this->value = std::move(value);
}
ASTNS::Param::Param(std::unique_ptr<Type> type, Token name, std::unique_ptr<Param> next)
{
    this->type = std::move(type);
    this->name = name;
    this->next = std::move(next);
}
ASTNS::Arg::Arg(std::unique_ptr<Expr> value, std::unique_ptr<Arg> next)
{
    this->value = std::move(value);
    this->next = std::move(next);
}
