#include "ast.h"
ASTNS::Program::Program(std::vector<std::unique_ptr<Decl>> &decls)
{
    for (auto &p : decls) decls.push_back(std::move(p));
}
ASTNS::BinaryExpr::BinaryExpr(std::unique_ptr<Expr> lhs, std::unique_ptr<Expr> rhs, Token op)
{
    lhs = std::move(lhs);
    rhs = std::move(rhs);
    op = op;
}
ASTNS::TernaryExpr::TernaryExpr(std::unique_ptr<Expr> condition, std::unique_ptr<Expr> trues, std::unique_ptr<Expr> falses)
{
    condition = std::move(condition);
    trues = std::move(trues);
    falses = std::move(falses);
}
ASTNS::UnaryExpr::UnaryExpr(std::unique_ptr<Expr> operand, Token op)
{
    operand = std::move(operand);
    op = op;
}
ASTNS::PrimaryExpr::PrimaryExpr(Token value)
{
    value = value;
}
ASTNS::AssignExpr::AssignExpr(std::unique_ptr<Expr> assignee, std::unique_ptr<Expr> value)
{
    assignee = std::move(assignee);
    value = std::move(value);
}
ASTNS::CallExpr::CallExpr(std::unique_ptr<Expr> func, std::unique_ptr<Arg> args)
{
    func = std::move(func);
    args = std::move(args);
}
ASTNS::LtoRVExpr::LtoRVExpr(std::unique_ptr<Expr> val)
{
    val = std::move(val);
}
ASTNS::BlockStmt::BlockStmt(std::vector<Stmt> &stmts)
{
    for (auto &p : stmts) stmts.push_back(std::move(p));
}
ASTNS::ExprStmt::ExprStmt(std::unique_ptr<Expr> expr)
{
    expr = std::move(expr);
}
ASTNS::ReturnStmt::ReturnStmt(std::unique_ptr<Expr> val)
{
    val = std::move(val);
}
ASTNS::VarStmt::VarStmt(std::unique_ptr<Type> type, Token name, std::unique_ptr<Expr> value)
{
    type = std::move(type);
    name = name;
    value = std::move(value);
}
ASTNS::VarRef::VarRef(Token var)
{
    var = var;
}
ASTNS::BaseType::BaseType(Token type)
{
    type = type;
}
ASTNS::FunctionDecl::FunctionDecl(std::unique_ptr<Type> rettype, Token name, std::unique_ptr<Param> params, std::unique_ptr<BlockStmt> block)
{
    rettype = std::move(rettype);
    name = name;
    params = std::move(params);
    block = std::move(block);
}
ASTNS::GlobalVarDecl::GlobalVarDecl(std::unique_ptr<Type> type, Token name, std::unique_ptr<Expr> value)
{
    type = std::move(type);
    name = name;
    value = std::move(value);
}
ASTNS::Param::Param(std::unique_ptr<Type> type, Token name, std::unique_ptr<Param> next)
{
    type = std::move(type);
    name = name;
    next = std::move(next);
}
ASTNS::Arg::Arg(std::unique_ptr<Expr> value, std::unique_ptr<Arg> next)
{
    value = std::move(value);
    next = std::move(next);
}
