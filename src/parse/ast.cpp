
// The following code was autogenerated - see the utils/ directory
#include "parse/ast.h"
ASTNS::Program::Program(std::vector<std::unique_ptr<Decl>> &decls): decls(std::move(decls)) {}
void ASTNS::Program::accept(ProgramVisitor *v) { v->visitProgram(this); }
ASTNS::BinaryExpr::BinaryExpr(std::unique_ptr<Expr> lhs, std::unique_ptr<Expr> rhs, Token op): lhs(std::move(lhs)), rhs(std::move(rhs)), op(op) {}
void ASTNS::BinaryExpr::accept(ExprVisitor *v) { v->visitBinaryExpr(this); }
ASTNS::TernaryExpr::TernaryExpr(std::unique_ptr<Expr> condition, std::unique_ptr<Expr> trues, std::unique_ptr<Expr> falses): condition(std::move(condition)), trues(std::move(trues)), falses(std::move(falses)) {}
void ASTNS::TernaryExpr::accept(ExprVisitor *v) { v->visitTernaryExpr(this); }
ASTNS::UnaryExpr::UnaryExpr(std::unique_ptr<Expr> operand, Token op): operand(std::move(operand)), op(op) {}
void ASTNS::UnaryExpr::accept(ExprVisitor *v) { v->visitUnaryExpr(this); }
ASTNS::PrimaryExpr::PrimaryExpr(Token value): value(value) {}
void ASTNS::PrimaryExpr::accept(ExprVisitor *v) { v->visitPrimaryExpr(this); }
ASTNS::CallExpr::CallExpr(std::unique_ptr<Expr> func, std::unique_ptr<Arg> args): func(std::move(func)), args(std::move(args)) {}
void ASTNS::CallExpr::accept(ExprVisitor *v) { v->visitCallExpr(this); }
ASTNS::BlockStmt::BlockStmt(std::vector<std::unique_ptr<Stmt>> &stmts): stmts(std::move(stmts)) {}
void ASTNS::BlockStmt::accept(StmtVisitor *v) { v->visitBlockStmt(this); }
ASTNS::ExprStmt::ExprStmt(std::unique_ptr<Expr> expr): expr(std::move(expr)) {}
void ASTNS::ExprStmt::accept(StmtVisitor *v) { v->visitExprStmt(this); }
ASTNS::ReturnStmt::ReturnStmt(std::unique_ptr<Expr> val): val(std::move(val)) {}
void ASTNS::ReturnStmt::accept(StmtVisitor *v) { v->visitReturnStmt(this); }
ASTNS::VarStmt::VarStmt(std::unique_ptr<Type> type, std::vector<std::unique_ptr<Expr>> &assignments): type(std::move(type)), assignments(std::move(assignments)) {}
void ASTNS::VarStmt::accept(StmtVisitor *v) { v->visitVarStmt(this); }
ASTNS::BaseType::BaseType(Token type): type(type) {}
void ASTNS::BaseType::accept(TypeVisitor *v) { v->visitBaseType(this); }
ASTNS::FunctionDecl::FunctionDecl(std::unique_ptr<Type> rettype, Token name, std::unique_ptr<Param> params, std::unique_ptr<BlockStmt> block): rettype(std::move(rettype)), name(name), params(std::move(params)), block(std::move(block)) {}
void ASTNS::FunctionDecl::accept(DeclVisitor *v) { v->visitFunctionDecl(this); }
ASTNS::GlobalVarDecl::GlobalVarDecl(std::unique_ptr<Type> type, std::vector<std::unique_ptr<Expr>> &assignments): type(std::move(type)), assignments(std::move(assignments)) {}
void ASTNS::GlobalVarDecl::accept(DeclVisitor *v) { v->visitGlobalVarDecl(this); }
ASTNS::Param::Param(std::unique_ptr<Type> type, Token name, std::unique_ptr<Param> next): type(std::move(type)), name(name), next(std::move(next)) {}
void ASTNS::Param::accept(ParamVisitor *v) { v->visitParam(this); }
ASTNS::Arg::Arg(std::unique_ptr<Expr> value, std::unique_ptr<Arg> next): value(std::move(value)), next(std::move(next)) {}
void ASTNS::Arg::accept(ArgVisitor *v) { v->visitArg(this); }
// This code was autogenerated - see the utils/ directory

