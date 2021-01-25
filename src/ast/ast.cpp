#include "ast/ast.h"
// ASTCPP START
ASTNS::AST::AST(File const &file): file(file) {}
ASTNS::CUB::CUB(File const &file): AST(file) {}
ASTNS::Decl::Decl(File const &file): AST(file) {}
ASTNS::ImplMember::ImplMember(File const &file): AST(file) {}
ASTNS::Stmt::Stmt(File const &file): AST(file) {}
ASTNS::Expr::Expr(File const &file): AST(file) {}
ASTNS::Type::Type(File const &file): AST(file) {}
ASTNS::ArgB::ArgB(File const &file): AST(file) {}
ASTNS::ParamB::ParamB(File const &file): AST(file) {}
ASTNS::VStmtIB::VStmtIB(File const &file): AST(file) {}
ASTNS::PathB::PathB(File const &file): AST(file) {}
ASTNS::ListB::ListB(File const &file): AST(file) {}
ASTNS::DeclList::DeclList(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::vector<std::unique_ptr<Decl>> decls): ListB(file), _start(start), _end(end), decls(std::move(decls)) {}
void ASTNS::DeclList::accept(ASTNS::ListBVisitor &v) { v.visit(*this); }
Maybe<Location const> const &ASTNS::DeclList::start() const { return _start; }
Maybe<Location const> const &ASTNS::DeclList::end() const { return _end; }
ASTNS::StmtList::StmtList(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::vector<std::unique_ptr<Stmt>> stmts): ListB(file), _start(start), _end(end), stmts(std::move(stmts)) {}
void ASTNS::StmtList::accept(ASTNS::ListBVisitor &v) { v.visit(*this); }
Maybe<Location const> const &ASTNS::StmtList::start() const { return _start; }
Maybe<Location const> const &ASTNS::StmtList::end() const { return _end; }
ASTNS::ParamList::ParamList(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::vector<std::unique_ptr<ParamB>> params): ListB(file), _start(start), _end(end), params(std::move(params)) {}
void ASTNS::ParamList::accept(ASTNS::ListBVisitor &v) { v.visit(*this); }
Maybe<Location const> const &ASTNS::ParamList::start() const { return _start; }
Maybe<Location const> const &ASTNS::ParamList::end() const { return _end; }
ASTNS::ArgList::ArgList(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::vector<std::unique_ptr<Arg>> args): ListB(file), _start(start), _end(end), args(std::move(args)) {}
void ASTNS::ArgList::accept(ASTNS::ListBVisitor &v) { v.visit(*this); }
Maybe<Location const> const &ASTNS::ArgList::start() const { return _start; }
Maybe<Location const> const &ASTNS::ArgList::end() const { return _end; }
ASTNS::VarStmtItemList::VarStmtItemList(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::vector<std::unique_ptr<VarStmtItem>> items): ListB(file), _start(start), _end(end), items(std::move(items)) {}
void ASTNS::VarStmtItemList::accept(ASTNS::ListBVisitor &v) { v.visit(*this); }
Maybe<Location const> const &ASTNS::VarStmtItemList::start() const { return _start; }
Maybe<Location const> const &ASTNS::VarStmtItemList::end() const { return _end; }
ASTNS::ImplMemberList::ImplMemberList(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::vector<std::unique_ptr<ImplMember>> members): ListB(file), _start(start), _end(end), members(std::move(members)) {}
void ASTNS::ImplMemberList::accept(ASTNS::ListBVisitor &v) { v.visit(*this); }
Maybe<Location const> const &ASTNS::ImplMemberList::start() const { return _start; }
Maybe<Location const> const &ASTNS::ImplMemberList::end() const { return _end; }
ASTNS::PureLocationB::PureLocationB(File const &file): AST(file) {}
ASTNS::PureLocation::PureLocation(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, int dummy): PureLocationB(file), _start(start), _end(end), dummy(std::move(dummy)) {}
void ASTNS::PureLocation::accept(ASTNS::PureLocationBVisitor &v) { v.visit(*this); }
Maybe<Location const> const &ASTNS::PureLocation::start() const { return _start; }
Maybe<Location const> const &ASTNS::PureLocation::end() const { return _end; }
ASTNS::ImplicitDecl::ImplicitDecl(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, int dummy): Decl(file), _start(start), _end(end), dummy(std::move(dummy)) {}
void ASTNS::ImplicitDecl::accept(ASTNS::DeclVisitor &v) { v.visit(*this); }
Maybe<Location const> const &ASTNS::ImplicitDecl::start() const { return _start; }
Maybe<Location const> const &ASTNS::ImplicitDecl::end() const { return _end; }
ASTNS::CU::CU(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::vector<std::unique_ptr<Decl>> decls): CUB(file), _start(start), _end(end), decls(std::move(decls)) {}
void ASTNS::CU::accept(ASTNS::CUBVisitor &v) { v.visit(*this); }
Maybe<Location const> const &ASTNS::CU::start() const { return _start; }
Maybe<Location const> const &ASTNS::CU::end() const { return _end; }
ASTNS::ImplDecl::ImplDecl(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::unique_ptr<Type> impl_for, std::vector<std::unique_ptr<ImplMember>> members): Decl(file), _start(start), _end(end), impl_for(std::move(impl_for)), members(std::move(members)) {}
void ASTNS::ImplDecl::accept(ASTNS::DeclVisitor &v) { v.visit(*this); }
Maybe<Location const> const &ASTNS::ImplDecl::start() const { return _start; }
Maybe<Location const> const &ASTNS::ImplDecl::end() const { return _end; }
ASTNS::FunctionDecl::FunctionDecl(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::unique_ptr<Type> retty, Token name, std::vector<std::unique_ptr<ParamB>> params, std::unique_ptr<Block> body): Decl(file), _start(start), _end(end), retty(std::move(retty)), name(std::move(name)), params(std::move(params)), body(std::move(body)) {}
void ASTNS::FunctionDecl::accept(ASTNS::DeclVisitor &v) { v.visit(*this); }
Maybe<Location const> const &ASTNS::FunctionDecl::start() const { return _start; }
Maybe<Location const> const &ASTNS::FunctionDecl::end() const { return _end; }
ASTNS::FunctionImplMember::FunctionImplMember(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::unique_ptr<FunctionDecl> fun): ImplMember(file), _start(start), _end(end), fun(std::move(fun)) {}
void ASTNS::FunctionImplMember::accept(ASTNS::ImplMemberVisitor &v) { v.visit(*this); }
Maybe<Location const> const &ASTNS::FunctionImplMember::start() const { return _start; }
Maybe<Location const> const &ASTNS::FunctionImplMember::end() const { return _end; }
ASTNS::VarStmt::VarStmt(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::vector<std::unique_ptr<VarStmtItem>> items): Stmt(file), _start(start), _end(end), items(std::move(items)) {}
void ASTNS::VarStmt::accept(ASTNS::StmtVisitor &v) { v.visit(*this); }
Maybe<Location const> const &ASTNS::VarStmt::start() const { return _start; }
Maybe<Location const> const &ASTNS::VarStmt::end() const { return _end; }
ASTNS::VarStmtItem::VarStmtItem(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::unique_ptr<Type> type, bool mut, Token name, Token equal, std::unique_ptr<Expr> expr): VStmtIB(file), _start(start), _end(end), type(std::move(type)), mut(std::move(mut)), name(std::move(name)), equal(std::move(equal)), expr(std::move(expr)) {}
void ASTNS::VarStmtItem::accept(ASTNS::VStmtIBVisitor &v) { v.visit(*this); }
Maybe<Location const> const &ASTNS::VarStmtItem::start() const { return _start; }
Maybe<Location const> const &ASTNS::VarStmtItem::end() const { return _end; }
ASTNS::ExprStmt::ExprStmt(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::unique_ptr<Expr> expr, bool suppress, Maybe<Location const> dot): Stmt(file), _start(start), _end(end), expr(std::move(expr)), suppress(std::move(suppress)), dot(std::move(dot)) {}
void ASTNS::ExprStmt::accept(ASTNS::StmtVisitor &v) { v.visit(*this); }
Maybe<Location const> const &ASTNS::ExprStmt::start() const { return _start; }
Maybe<Location const> const &ASTNS::ExprStmt::end() const { return _end; }
ASTNS::RetStmt::RetStmt(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::unique_ptr<Expr> expr): Stmt(file), _start(start), _end(end), expr(std::move(expr)) {}
void ASTNS::RetStmt::accept(ASTNS::StmtVisitor &v) { v.visit(*this); }
Maybe<Location const> const &ASTNS::RetStmt::start() const { return _start; }
Maybe<Location const> const &ASTNS::RetStmt::end() const { return _end; }
ASTNS::PathType::PathType(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::unique_ptr<Path> path): Type(file), _start(start), _end(end), path(std::move(path)) {}
void ASTNS::PathType::accept(ASTNS::TypeVisitor &v) { v.visit(*this); }
Maybe<Location const> const &ASTNS::PathType::start() const { return _start; }
Maybe<Location const> const &ASTNS::PathType::end() const { return _end; }
ASTNS::PointerType::PointerType(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, bool mut, std::unique_ptr<Type> type): Type(file), _start(start), _end(end), mut(std::move(mut)), type(std::move(type)) {}
void ASTNS::PointerType::accept(ASTNS::TypeVisitor &v) { v.visit(*this); }
Maybe<Location const> const &ASTNS::PointerType::start() const { return _start; }
Maybe<Location const> const &ASTNS::PointerType::end() const { return _end; }
ASTNS::ThisType::ThisType(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, Token th): Type(file), _start(start), _end(end), th(std::move(th)) {}
void ASTNS::ThisType::accept(ASTNS::TypeVisitor &v) { v.visit(*this); }
Maybe<Location const> const &ASTNS::ThisType::start() const { return _start; }
Maybe<Location const> const &ASTNS::ThisType::end() const { return _end; }
ASTNS::Arg::Arg(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::unique_ptr<Expr> expr): ArgB(file), _start(start), _end(end), expr(std::move(expr)) {}
void ASTNS::Arg::accept(ASTNS::ArgBVisitor &v) { v.visit(*this); }
Maybe<Location const> const &ASTNS::Arg::start() const { return _start; }
Maybe<Location const> const &ASTNS::Arg::end() const { return _end; }
ASTNS::Param::Param(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::unique_ptr<Type> type, Token name, bool mut): ParamB(file), _start(start), _end(end), type(std::move(type)), name(std::move(name)), mut(std::move(mut)) {}
void ASTNS::Param::accept(ASTNS::ParamBVisitor &v) { v.visit(*this); }
Maybe<Location const> const &ASTNS::Param::start() const { return _start; }
Maybe<Location const> const &ASTNS::Param::end() const { return _end; }
ASTNS::ThisParam::ThisParam(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, bool ptr, bool mut): ParamB(file), _start(start), _end(end), ptr(std::move(ptr)), mut(std::move(mut)) {}
void ASTNS::ThisParam::accept(ASTNS::ParamBVisitor &v) { v.visit(*this); }
Maybe<Location const> const &ASTNS::ThisParam::start() const { return _start; }
Maybe<Location const> const &ASTNS::ThisParam::end() const { return _end; }
ASTNS::Block::Block(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::vector<std::unique_ptr<Stmt>> stmts): Expr(file), _start(start), _end(end), stmts(std::move(stmts)) {}
void ASTNS::Block::accept(ASTNS::ExprVisitor &v) { v.visit(*this); }
Maybe<Location const> const &ASTNS::Block::start() const { return _start; }
Maybe<Location const> const &ASTNS::Block::end() const { return _end; }
ASTNS::IfExpr::IfExpr(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, Token iftok, Token elsetok, std::unique_ptr<Expr> cond, std::unique_ptr<Expr> trues, std::unique_ptr<Expr> falses): Expr(file), _start(start), _end(end), iftok(std::move(iftok)), elsetok(std::move(elsetok)), cond(std::move(cond)), trues(std::move(trues)), falses(std::move(falses)) {}
void ASTNS::IfExpr::accept(ASTNS::ExprVisitor &v) { v.visit(*this); }
Maybe<Location const> const &ASTNS::IfExpr::start() const { return _start; }
Maybe<Location const> const &ASTNS::IfExpr::end() const { return _end; }
ASTNS::WhileExpr::WhileExpr(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::unique_ptr<Expr> cond, std::unique_ptr<Expr> body): Expr(file), _start(start), _end(end), cond(std::move(cond)), body(std::move(body)) {}
void ASTNS::WhileExpr::accept(ASTNS::ExprVisitor &v) { v.visit(*this); }
Maybe<Location const> const &ASTNS::WhileExpr::start() const { return _start; }
Maybe<Location const> const &ASTNS::WhileExpr::end() const { return _end; }
ASTNS::AssignmentExpr::AssignmentExpr(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::unique_ptr<Expr> target, Token equal, std::unique_ptr<Expr> expr): Expr(file), _start(start), _end(end), target(std::move(target)), equal(std::move(equal)), expr(std::move(expr)) {}
void ASTNS::AssignmentExpr::accept(ASTNS::ExprVisitor &v) { v.visit(*this); }
Maybe<Location const> const &ASTNS::AssignmentExpr::start() const { return _start; }
Maybe<Location const> const &ASTNS::AssignmentExpr::end() const { return _end; }
ASTNS::ShortCircuitExpr::ShortCircuitExpr(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::unique_ptr<Expr> lhs, Token op, std::unique_ptr<Expr> rhs): Expr(file), _start(start), _end(end), lhs(std::move(lhs)), op(std::move(op)), rhs(std::move(rhs)) {}
void ASTNS::ShortCircuitExpr::accept(ASTNS::ExprVisitor &v) { v.visit(*this); }
Maybe<Location const> const &ASTNS::ShortCircuitExpr::start() const { return _start; }
Maybe<Location const> const &ASTNS::ShortCircuitExpr::end() const { return _end; }
ASTNS::BinaryExpr::BinaryExpr(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::unique_ptr<Expr> lhs, Token op, std::unique_ptr<Expr> rhs): Expr(file), _start(start), _end(end), lhs(std::move(lhs)), op(std::move(op)), rhs(std::move(rhs)) {}
void ASTNS::BinaryExpr::accept(ASTNS::ExprVisitor &v) { v.visit(*this); }
Maybe<Location const> const &ASTNS::BinaryExpr::start() const { return _start; }
Maybe<Location const> const &ASTNS::BinaryExpr::end() const { return _end; }
ASTNS::CastExpr::CastExpr(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::unique_ptr<Type> type, std::unique_ptr<Expr> expr): Expr(file), _start(start), _end(end), type(std::move(type)), expr(std::move(expr)) {}
void ASTNS::CastExpr::accept(ASTNS::ExprVisitor &v) { v.visit(*this); }
Maybe<Location const> const &ASTNS::CastExpr::start() const { return _start; }
Maybe<Location const> const &ASTNS::CastExpr::end() const { return _end; }
ASTNS::UnaryExpr::UnaryExpr(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, Token op, std::unique_ptr<Expr> expr): Expr(file), _start(start), _end(end), op(std::move(op)), expr(std::move(expr)) {}
void ASTNS::UnaryExpr::accept(ASTNS::ExprVisitor &v) { v.visit(*this); }
Maybe<Location const> const &ASTNS::UnaryExpr::start() const { return _start; }
Maybe<Location const> const &ASTNS::UnaryExpr::end() const { return _end; }
ASTNS::AddrofExpr::AddrofExpr(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, Token op, std::unique_ptr<Expr> expr, bool mut): Expr(file), _start(start), _end(end), op(std::move(op)), expr(std::move(expr)), mut(std::move(mut)) {}
void ASTNS::AddrofExpr::accept(ASTNS::ExprVisitor &v) { v.visit(*this); }
Maybe<Location const> const &ASTNS::AddrofExpr::start() const { return _start; }
Maybe<Location const> const &ASTNS::AddrofExpr::end() const { return _end; }
ASTNS::DerefExpr::DerefExpr(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, Token op, std::unique_ptr<Expr> expr): Expr(file), _start(start), _end(end), op(std::move(op)), expr(std::move(expr)) {}
void ASTNS::DerefExpr::accept(ASTNS::ExprVisitor &v) { v.visit(*this); }
Maybe<Location const> const &ASTNS::DerefExpr::start() const { return _start; }
Maybe<Location const> const &ASTNS::DerefExpr::end() const { return _end; }
ASTNS::CallExpr::CallExpr(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::unique_ptr<Expr> callee, Token oparn, std::vector<std::unique_ptr<Arg>> args): Expr(file), _start(start), _end(end), callee(std::move(callee)), oparn(std::move(oparn)), args(std::move(args)) {}
void ASTNS::CallExpr::accept(ASTNS::ExprVisitor &v) { v.visit(*this); }
Maybe<Location const> const &ASTNS::CallExpr::start() const { return _start; }
Maybe<Location const> const &ASTNS::CallExpr::end() const { return _end; }
ASTNS::FieldAccessExpr::FieldAccessExpr(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::unique_ptr<Expr> operand, Token dot, Token field): Expr(file), _start(start), _end(end), operand(std::move(operand)), dot(std::move(dot)), field(std::move(field)) {}
void ASTNS::FieldAccessExpr::accept(ASTNS::ExprVisitor &v) { v.visit(*this); }
Maybe<Location const> const &ASTNS::FieldAccessExpr::start() const { return _start; }
Maybe<Location const> const &ASTNS::FieldAccessExpr::end() const { return _end; }
ASTNS::MethodCallExpr::MethodCallExpr(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::unique_ptr<Expr> operand, Token dot, Token method, Token oparn, std::vector<std::unique_ptr<Arg>> args): Expr(file), _start(start), _end(end), operand(std::move(operand)), dot(std::move(dot)), method(std::move(method)), oparn(std::move(oparn)), args(std::move(args)) {}
void ASTNS::MethodCallExpr::accept(ASTNS::ExprVisitor &v) { v.visit(*this); }
Maybe<Location const> const &ASTNS::MethodCallExpr::start() const { return _start; }
Maybe<Location const> const &ASTNS::MethodCallExpr::end() const { return _end; }
ASTNS::PrimaryExpr::PrimaryExpr(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, Token value): Expr(file), _start(start), _end(end), value(std::move(value)) {}
void ASTNS::PrimaryExpr::accept(ASTNS::ExprVisitor &v) { v.visit(*this); }
Maybe<Location const> const &ASTNS::PrimaryExpr::start() const { return _start; }
Maybe<Location const> const &ASTNS::PrimaryExpr::end() const { return _end; }
ASTNS::PathExpr::PathExpr(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::unique_ptr<Path> path): Expr(file), _start(start), _end(end), path(std::move(path)) {}
void ASTNS::PathExpr::accept(ASTNS::ExprVisitor &v) { v.visit(*this); }
Maybe<Location const> const &ASTNS::PathExpr::start() const { return _start; }
Maybe<Location const> const &ASTNS::PathExpr::end() const { return _end; }
ASTNS::Path::Path(File const &file, Maybe<Location const> const &start, Maybe<Location const> const &end, std::vector<Token> segments): PathB(file), _start(start), _end(end), segments(std::move(segments)) {}
void ASTNS::Path::accept(ASTNS::PathBVisitor &v) { v.visit(*this); }
Maybe<Location const> const &ASTNS::Path::start() const { return _start; }
Maybe<Location const> const &ASTNS::Path::end() const { return _end; }
// ASTCPP END
