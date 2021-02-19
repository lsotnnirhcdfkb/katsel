#include "ast/ast.h"
// ASTCPP START
ASTNS::DeclList::DeclList(Maybe<Span const> const &span, std::vector<std::unique_ptr<Decl>> decls): _span(span), decls(std::move(decls)) {}
void ASTNS::DeclList::ast_accept(ASTNS::ListBVisitor &v) { v.ast_visit(*this); }
Maybe<Span const> const &ASTNS::DeclList::span() const { return _span; }
ASTNS::StmtList::StmtList(Maybe<Span const> const &span, std::vector<std::unique_ptr<Stmt>> stmts): _span(span), stmts(std::move(stmts)) {}
void ASTNS::StmtList::ast_accept(ASTNS::ListBVisitor &v) { v.ast_visit(*this); }
Maybe<Span const> const &ASTNS::StmtList::span() const { return _span; }
ASTNS::ParamList::ParamList(Maybe<Span const> const &span, std::vector<std::unique_ptr<ParamB>> params): _span(span), params(std::move(params)) {}
void ASTNS::ParamList::ast_accept(ASTNS::ListBVisitor &v) { v.ast_visit(*this); }
Maybe<Span const> const &ASTNS::ParamList::span() const { return _span; }
ASTNS::ArgList::ArgList(Maybe<Span const> const &span, std::vector<std::unique_ptr<Arg>> args): _span(span), args(std::move(args)) {}
void ASTNS::ArgList::ast_accept(ASTNS::ListBVisitor &v) { v.ast_visit(*this); }
Maybe<Span const> const &ASTNS::ArgList::span() const { return _span; }
ASTNS::ImplMemberList::ImplMemberList(Maybe<Span const> const &span, std::vector<std::unique_ptr<ImplMember>> members): _span(span), members(std::move(members)) {}
void ASTNS::ImplMemberList::ast_accept(ASTNS::ListBVisitor &v) { v.ast_visit(*this); }
Maybe<Span const> const &ASTNS::ImplMemberList::span() const { return _span; }
ASTNS::PureLocation::PureLocation(Maybe<Span const> const &span, int dummy): _span(span), dummy(std::move(dummy)) {}
void ASTNS::PureLocation::ast_accept(ASTNS::PureLocationBVisitor &v) { v.ast_visit(*this); }
Maybe<Span const> const &ASTNS::PureLocation::span() const { return _span; }
ASTNS::ImplicitDecl::ImplicitDecl(Maybe<Span const> const &span, int dummy): _span(span), dummy(std::move(dummy)) {}
void ASTNS::ImplicitDecl::ast_accept(ASTNS::DeclVisitor &v) { v.ast_visit(*this); }
Maybe<Span const> const &ASTNS::ImplicitDecl::span() const { return _span; }
ASTNS::CU::CU(Maybe<Span const> const &span, std::vector<std::unique_ptr<Decl>> decls): _span(span), decls(std::move(decls)) {}
void ASTNS::CU::ast_accept(ASTNS::CUBVisitor &v) { v.ast_visit(*this); }
Maybe<Span const> const &ASTNS::CU::span() const { return _span; }
ASTNS::ImplDecl::ImplDecl(Maybe<Span const> const &span, std::unique_ptr<Type> impl_for, std::vector<std::unique_ptr<ImplMember>> members): _span(span), impl_for(std::move(impl_for)), members(std::move(members)) {}
void ASTNS::ImplDecl::ast_accept(ASTNS::DeclVisitor &v) { v.ast_visit(*this); }
Maybe<Span const> const &ASTNS::ImplDecl::span() const { return _span; }
ASTNS::FunctionDecl::FunctionDecl(Maybe<Span const> const &span, std::unique_ptr<Type> retty, Located<Tokens::Identifier> name, std::vector<std::unique_ptr<ParamB>> params, std::unique_ptr<Block> body): _span(span), retty(std::move(retty)), name(std::move(name)), params(std::move(params)), body(std::move(body)) {}
void ASTNS::FunctionDecl::ast_accept(ASTNS::DeclVisitor &v) { v.ast_visit(*this); }
Maybe<Span const> const &ASTNS::FunctionDecl::span() const { return _span; }
ASTNS::FunctionImplMember::FunctionImplMember(Maybe<Span const> const &span, std::unique_ptr<FunctionDecl> fun): _span(span), fun(std::move(fun)) {}
void ASTNS::FunctionImplMember::ast_accept(ASTNS::ImplMemberVisitor &v) { v.ast_visit(*this); }
Maybe<Span const> const &ASTNS::FunctionImplMember::span() const { return _span; }
ASTNS::VarStmt::VarStmt(Maybe<Span const> const &span, std::unique_ptr<Type> type, bool mut, Located<Tokens::Identifier> name, Maybe<Located<Tokens::Equal>> equal, std::unique_ptr<Expr> expr): _span(span), type(std::move(type)), mut(std::move(mut)), name(std::move(name)), equal(std::move(equal)), expr(std::move(expr)) {}
void ASTNS::VarStmt::ast_accept(ASTNS::StmtVisitor &v) { v.ast_visit(*this); }
Maybe<Span const> const &ASTNS::VarStmt::span() const { return _span; }
ASTNS::ExprStmt::ExprStmt(Maybe<Span const> const &span, std::unique_ptr<Expr> expr): _span(span), expr(std::move(expr)) {}
void ASTNS::ExprStmt::ast_accept(ASTNS::StmtVisitor &v) { v.ast_visit(*this); }
Maybe<Span const> const &ASTNS::ExprStmt::span() const { return _span; }
ASTNS::RetStmt::RetStmt(Maybe<Span const> const &span, std::unique_ptr<Expr> expr): _span(span), expr(std::move(expr)) {}
void ASTNS::RetStmt::ast_accept(ASTNS::StmtVisitor &v) { v.ast_visit(*this); }
Maybe<Span const> const &ASTNS::RetStmt::span() const { return _span; }
ASTNS::PathType::PathType(Maybe<Span const> const &span, std::unique_ptr<Path> path): _span(span), path(std::move(path)) {}
void ASTNS::PathType::ast_accept(ASTNS::TypeVisitor &v) { v.ast_visit(*this); }
Maybe<Span const> const &ASTNS::PathType::span() const { return _span; }
ASTNS::PointerType::PointerType(Maybe<Span const> const &span, bool mut, std::unique_ptr<Type> type): _span(span), mut(std::move(mut)), type(std::move(type)) {}
void ASTNS::PointerType::ast_accept(ASTNS::TypeVisitor &v) { v.ast_visit(*this); }
Maybe<Span const> const &ASTNS::PointerType::span() const { return _span; }
ASTNS::ThisType::ThisType(Maybe<Span const> const &span, Located<Tokens::This> th): _span(span), th(std::move(th)) {}
void ASTNS::ThisType::ast_accept(ASTNS::TypeVisitor &v) { v.ast_visit(*this); }
Maybe<Span const> const &ASTNS::ThisType::span() const { return _span; }
ASTNS::Arg::Arg(Maybe<Span const> const &span, std::unique_ptr<Expr> expr): _span(span), expr(std::move(expr)) {}
void ASTNS::Arg::ast_accept(ASTNS::ArgBVisitor &v) { v.ast_visit(*this); }
Maybe<Span const> const &ASTNS::Arg::span() const { return _span; }
ASTNS::Param::Param(Maybe<Span const> const &span, std::unique_ptr<Type> type, Located<Tokens::Identifier> name, bool mut): _span(span), type(std::move(type)), name(std::move(name)), mut(std::move(mut)) {}
void ASTNS::Param::ast_accept(ASTNS::ParamBVisitor &v) { v.ast_visit(*this); }
Maybe<Span const> const &ASTNS::Param::span() const { return _span; }
ASTNS::ThisParam::ThisParam(Maybe<Span const> const &span, bool ptr, bool mut): _span(span), ptr(std::move(ptr)), mut(std::move(mut)) {}
void ASTNS::ThisParam::ast_accept(ASTNS::ParamBVisitor &v) { v.ast_visit(*this); }
Maybe<Span const> const &ASTNS::ThisParam::span() const { return _span; }
ASTNS::Block::Block(Maybe<Span const> const &span, std::vector<std::unique_ptr<Stmt>> stmts): _span(span), stmts(std::move(stmts)) {}
void ASTNS::Block::ast_accept(ASTNS::ExprVisitor &v) { v.ast_visit(*this); }
Maybe<Span const> const &ASTNS::Block::span() const { return _span; }
ASTNS::IfExpr::IfExpr(Maybe<Span const> const &span, Located<Tokens::If> iftok, Maybe<Located<Tokens::Else>> elsetok, std::unique_ptr<Expr> cond, std::unique_ptr<Expr> trues, std::unique_ptr<Expr> falses): _span(span), iftok(std::move(iftok)), elsetok(std::move(elsetok)), cond(std::move(cond)), trues(std::move(trues)), falses(std::move(falses)) {}
void ASTNS::IfExpr::ast_accept(ASTNS::ExprVisitor &v) { v.ast_visit(*this); }
Maybe<Span const> const &ASTNS::IfExpr::span() const { return _span; }
ASTNS::WhileExpr::WhileExpr(Maybe<Span const> const &span, std::unique_ptr<Expr> cond, std::unique_ptr<Expr> body): _span(span), cond(std::move(cond)), body(std::move(body)) {}
void ASTNS::WhileExpr::ast_accept(ASTNS::ExprVisitor &v) { v.ast_visit(*this); }
Maybe<Span const> const &ASTNS::WhileExpr::span() const { return _span; }
ASTNS::AssignmentExpr::AssignmentExpr(Maybe<Span const> const &span, std::unique_ptr<Expr> target, Located<AssignOperator> equal, std::unique_ptr<Expr> expr): _span(span), target(std::move(target)), equal(std::move(equal)), expr(std::move(expr)) {}
void ASTNS::AssignmentExpr::ast_accept(ASTNS::ExprVisitor &v) { v.ast_visit(*this); }
Maybe<Span const> const &ASTNS::AssignmentExpr::span() const { return _span; }
ASTNS::ShortCircuitExpr::ShortCircuitExpr(Maybe<Span const> const &span, std::unique_ptr<Expr> lhs, Located<ShortCircuitOperator> op, std::unique_ptr<Expr> rhs): _span(span), lhs(std::move(lhs)), op(std::move(op)), rhs(std::move(rhs)) {}
void ASTNS::ShortCircuitExpr::ast_accept(ASTNS::ExprVisitor &v) { v.ast_visit(*this); }
Maybe<Span const> const &ASTNS::ShortCircuitExpr::span() const { return _span; }
ASTNS::BinaryExpr::BinaryExpr(Maybe<Span const> const &span, std::unique_ptr<Expr> lhs, Located<BinaryOperator> op, std::unique_ptr<Expr> rhs): _span(span), lhs(std::move(lhs)), op(std::move(op)), rhs(std::move(rhs)) {}
void ASTNS::BinaryExpr::ast_accept(ASTNS::ExprVisitor &v) { v.ast_visit(*this); }
Maybe<Span const> const &ASTNS::BinaryExpr::span() const { return _span; }
ASTNS::CastExpr::CastExpr(Maybe<Span const> const &span, std::unique_ptr<Type> type, std::unique_ptr<Expr> expr): _span(span), type(std::move(type)), expr(std::move(expr)) {}
void ASTNS::CastExpr::ast_accept(ASTNS::ExprVisitor &v) { v.ast_visit(*this); }
Maybe<Span const> const &ASTNS::CastExpr::span() const { return _span; }
ASTNS::UnaryExpr::UnaryExpr(Maybe<Span const> const &span, Located<UnaryOperator> op, std::unique_ptr<Expr> expr): _span(span), op(std::move(op)), expr(std::move(expr)) {}
void ASTNS::UnaryExpr::ast_accept(ASTNS::ExprVisitor &v) { v.ast_visit(*this); }
Maybe<Span const> const &ASTNS::UnaryExpr::span() const { return _span; }
ASTNS::AddrofExpr::AddrofExpr(Maybe<Span const> const &span, Located<Tokens::Amper> op, std::unique_ptr<Expr> expr, bool mut): _span(span), op(std::move(op)), expr(std::move(expr)), mut(std::move(mut)) {}
void ASTNS::AddrofExpr::ast_accept(ASTNS::ExprVisitor &v) { v.ast_visit(*this); }
Maybe<Span const> const &ASTNS::AddrofExpr::span() const { return _span; }
ASTNS::DerefExpr::DerefExpr(Maybe<Span const> const &span, Located<Tokens::Star> op, std::unique_ptr<Expr> expr): _span(span), op(std::move(op)), expr(std::move(expr)) {}
void ASTNS::DerefExpr::ast_accept(ASTNS::ExprVisitor &v) { v.ast_visit(*this); }
Maybe<Span const> const &ASTNS::DerefExpr::span() const { return _span; }
ASTNS::CallExpr::CallExpr(Maybe<Span const> const &span, std::unique_ptr<Expr> callee, Located<Tokens::OParen> oparn, std::vector<std::unique_ptr<Arg>> args): _span(span), callee(std::move(callee)), oparn(std::move(oparn)), args(std::move(args)) {}
void ASTNS::CallExpr::ast_accept(ASTNS::ExprVisitor &v) { v.ast_visit(*this); }
Maybe<Span const> const &ASTNS::CallExpr::span() const { return _span; }
ASTNS::FieldAccessExpr::FieldAccessExpr(Maybe<Span const> const &span, std::unique_ptr<Expr> operand, Located<Tokens::Period> dot, Located<Tokens::Identifier> field): _span(span), operand(std::move(operand)), dot(std::move(dot)), field(std::move(field)) {}
void ASTNS::FieldAccessExpr::ast_accept(ASTNS::ExprVisitor &v) { v.ast_visit(*this); }
Maybe<Span const> const &ASTNS::FieldAccessExpr::span() const { return _span; }
ASTNS::MethodCallExpr::MethodCallExpr(Maybe<Span const> const &span, std::unique_ptr<Expr> operand, Located<Tokens::Period> dot, Located<Tokens::Identifier> method, Located<Tokens::OParen> oparn, std::vector<std::unique_ptr<Arg>> args): _span(span), operand(std::move(operand)), dot(std::move(dot)), method(std::move(method)), oparn(std::move(oparn)), args(std::move(args)) {}
void ASTNS::MethodCallExpr::ast_accept(ASTNS::ExprVisitor &v) { v.ast_visit(*this); }
Maybe<Span const> const &ASTNS::MethodCallExpr::span() const { return _span; }
ASTNS::BoolLit::BoolLit(Maybe<Span const> const &span, Located<Tokens::BoolLit> val): _span(span), val(std::move(val)) {}
void ASTNS::BoolLit::ast_accept(ASTNS::ExprVisitor &v) { v.ast_visit(*this); }
Maybe<Span const> const &ASTNS::BoolLit::span() const { return _span; }
ASTNS::FloatLit::FloatLit(Maybe<Span const> const &span, Located<Tokens::FloatLit> val): _span(span), val(std::move(val)) {}
void ASTNS::FloatLit::ast_accept(ASTNS::ExprVisitor &v) { v.ast_visit(*this); }
Maybe<Span const> const &ASTNS::FloatLit::span() const { return _span; }
ASTNS::IntLit::IntLit(Maybe<Span const> const &span, Located<Tokens::IntLit> val): _span(span), val(std::move(val)) {}
void ASTNS::IntLit::ast_accept(ASTNS::ExprVisitor &v) { v.ast_visit(*this); }
Maybe<Span const> const &ASTNS::IntLit::span() const { return _span; }
ASTNS::CharLit::CharLit(Maybe<Span const> const &span, Located<Tokens::CharLit> val): _span(span), val(std::move(val)) {}
void ASTNS::CharLit::ast_accept(ASTNS::ExprVisitor &v) { v.ast_visit(*this); }
Maybe<Span const> const &ASTNS::CharLit::span() const { return _span; }
ASTNS::StringLit::StringLit(Maybe<Span const> const &span, Located<Tokens::StringLit> val): _span(span), val(std::move(val)) {}
void ASTNS::StringLit::ast_accept(ASTNS::ExprVisitor &v) { v.ast_visit(*this); }
Maybe<Span const> const &ASTNS::StringLit::span() const { return _span; }
ASTNS::ThisExpr::ThisExpr(Maybe<Span const> const &span, Located<Tokens::This> tok): _span(span), tok(std::move(tok)) {}
void ASTNS::ThisExpr::ast_accept(ASTNS::ExprVisitor &v) { v.ast_visit(*this); }
Maybe<Span const> const &ASTNS::ThisExpr::span() const { return _span; }
ASTNS::PathExpr::PathExpr(Maybe<Span const> const &span, std::unique_ptr<Path> path): _span(span), path(std::move(path)) {}
void ASTNS::PathExpr::ast_accept(ASTNS::ExprVisitor &v) { v.ast_visit(*this); }
Maybe<Span const> const &ASTNS::PathExpr::span() const { return _span; }
ASTNS::Path::Path(Maybe<Span const> const &span, std::vector<Located<Tokens::Identifier>> segments): _span(span), segments(std::move(segments)) {}
void ASTNS::Path::ast_accept(ASTNS::PathBVisitor &v) { v.ast_visit(*this); }
Maybe<Span const> const &ASTNS::Path::span() const { return _span; }
// ASTCPP END
