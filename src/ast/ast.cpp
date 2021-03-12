#include "ast/ast.h"
// ASTCPP START
ASTNS::CU::CU(Span const &span, std::vector<std::unique_ptr<Decl>> decls): _span(span), decls(std::move(decls)) {}
void ASTNS::CU::ast_accept(ASTNS::CUBVisitor &v) { v.ast_visit(*this); }
Span const &ASTNS::CU::span() const { return _span; }
ASTNS::ImplDecl::ImplDecl(Span const &span, std::unique_ptr<Type> impl_for, std::vector<std::unique_ptr<ImplMember>> members): _span(span), impl_for(std::move(impl_for)), members(std::move(members)) {}
void ASTNS::ImplDecl::ast_accept(ASTNS::DeclVisitor &v) { v.ast_visit(*this); }
Span const &ASTNS::ImplDecl::span() const { return _span; }
ASTNS::FunctionDecl::FunctionDecl(Span const &span, std::unique_ptr<Type> retty, Located<Token> name, std::vector<std::unique_ptr<ParamB>> params, std::unique_ptr<Block> body): _span(span), retty(std::move(retty)), name(std::move(name)), params(std::move(params)), body(std::move(body)) {}
void ASTNS::FunctionDecl::ast_accept(ASTNS::DeclVisitor &v) { v.ast_visit(*this); }
Span const &ASTNS::FunctionDecl::span() const { return _span; }
ASTNS::FunctionImplMember::FunctionImplMember(Span const &span, std::unique_ptr<FunctionDecl> fun): _span(span), fun(std::move(fun)) {}
void ASTNS::FunctionImplMember::ast_accept(ASTNS::ImplMemberVisitor &v) { v.ast_visit(*this); }
Span const &ASTNS::FunctionImplMember::span() const { return _span; }
ASTNS::VarStmt::VarStmt(Span const &span, std::unique_ptr<Type> type, bool mut, Located<Token> name, Maybe<Located<Token>> equal, std::unique_ptr<Expr> expr): _span(span), type(std::move(type)), mut(std::move(mut)), name(std::move(name)), equal(std::move(equal)), expr(std::move(expr)) {}
void ASTNS::VarStmt::ast_accept(ASTNS::StmtVisitor &v) { v.ast_visit(*this); }
Span const &ASTNS::VarStmt::span() const { return _span; }
ASTNS::ExprStmt::ExprStmt(Span const &span, std::unique_ptr<Expr> expr): _span(span), expr(std::move(expr)) {}
void ASTNS::ExprStmt::ast_accept(ASTNS::StmtVisitor &v) { v.ast_visit(*this); }
Span const &ASTNS::ExprStmt::span() const { return _span; }
ASTNS::RetStmt::RetStmt(Span const &span, std::unique_ptr<Expr> expr): _span(span), expr(std::move(expr)) {}
void ASTNS::RetStmt::ast_accept(ASTNS::StmtVisitor &v) { v.ast_visit(*this); }
Span const &ASTNS::RetStmt::span() const { return _span; }
ASTNS::PathType::PathType(Span const &span, std::unique_ptr<Path> path): _span(span), path(std::move(path)) {}
void ASTNS::PathType::ast_accept(ASTNS::TypeVisitor &v) { v.ast_visit(*this); }
Span const &ASTNS::PathType::span() const { return _span; }
ASTNS::PointerType::PointerType(Span const &span, bool mut, std::unique_ptr<Type> type): _span(span), mut(std::move(mut)), type(std::move(type)) {}
void ASTNS::PointerType::ast_accept(ASTNS::TypeVisitor &v) { v.ast_visit(*this); }
Span const &ASTNS::PointerType::span() const { return _span; }
ASTNS::ThisType::ThisType(Span const &span, Located<Token> th): _span(span), th(std::move(th)) {}
void ASTNS::ThisType::ast_accept(ASTNS::TypeVisitor &v) { v.ast_visit(*this); }
Span const &ASTNS::ThisType::span() const { return _span; }
ASTNS::Param::Param(Span const &span, std::unique_ptr<Type> type, Located<Token> name, bool mut): _span(span), type(std::move(type)), name(std::move(name)), mut(std::move(mut)) {}
void ASTNS::Param::ast_accept(ASTNS::ParamBVisitor &v) { v.ast_visit(*this); }
Span const &ASTNS::Param::span() const { return _span; }
ASTNS::ThisParam::ThisParam(Span const &span, bool ptr, bool mut): _span(span), ptr(std::move(ptr)), mut(std::move(mut)) {}
void ASTNS::ThisParam::ast_accept(ASTNS::ParamBVisitor &v) { v.ast_visit(*this); }
Span const &ASTNS::ThisParam::span() const { return _span; }
ASTNS::Block::Block(Span const &span, std::vector<std::unique_ptr<Stmt>> stmts): _span(span), stmts(std::move(stmts)) {}
void ASTNS::Block::ast_accept(ASTNS::ExprVisitor &v) { v.ast_visit(*this); }
Span const &ASTNS::Block::span() const { return _span; }
ASTNS::IfExpr::IfExpr(Span const &span, Located<Token> iftok, Maybe<Located<Token>> elsetok, std::unique_ptr<Expr> cond, std::unique_ptr<Expr> trues, std::unique_ptr<Expr> falses): _span(span), iftok(std::move(iftok)), elsetok(std::move(elsetok)), cond(std::move(cond)), trues(std::move(trues)), falses(std::move(falses)) {}
void ASTNS::IfExpr::ast_accept(ASTNS::ExprVisitor &v) { v.ast_visit(*this); }
Span const &ASTNS::IfExpr::span() const { return _span; }
ASTNS::WhileExpr::WhileExpr(Span const &span, std::unique_ptr<Expr> cond, std::unique_ptr<Expr> body): _span(span), cond(std::move(cond)), body(std::move(body)) {}
void ASTNS::WhileExpr::ast_accept(ASTNS::ExprVisitor &v) { v.ast_visit(*this); }
Span const &ASTNS::WhileExpr::span() const { return _span; }
ASTNS::AssignmentExpr::AssignmentExpr(Span const &span, std::unique_ptr<Expr> target, Located<AssignOperator> equal, std::unique_ptr<Expr> expr): _span(span), target(std::move(target)), equal(std::move(equal)), expr(std::move(expr)) {}
void ASTNS::AssignmentExpr::ast_accept(ASTNS::ExprVisitor &v) { v.ast_visit(*this); }
Span const &ASTNS::AssignmentExpr::span() const { return _span; }
ASTNS::ShortCircuitExpr::ShortCircuitExpr(Span const &span, std::unique_ptr<Expr> lhs, Located<ShortCircuitOperator> op, std::unique_ptr<Expr> rhs): _span(span), lhs(std::move(lhs)), op(std::move(op)), rhs(std::move(rhs)) {}
void ASTNS::ShortCircuitExpr::ast_accept(ASTNS::ExprVisitor &v) { v.ast_visit(*this); }
Span const &ASTNS::ShortCircuitExpr::span() const { return _span; }
ASTNS::BinaryExpr::BinaryExpr(Span const &span, std::unique_ptr<Expr> lhs, Located<BinaryOperator> op, std::unique_ptr<Expr> rhs): _span(span), lhs(std::move(lhs)), op(std::move(op)), rhs(std::move(rhs)) {}
void ASTNS::BinaryExpr::ast_accept(ASTNS::ExprVisitor &v) { v.ast_visit(*this); }
Span const &ASTNS::BinaryExpr::span() const { return _span; }
ASTNS::CastExpr::CastExpr(Span const &span, std::unique_ptr<Type> type, std::unique_ptr<Expr> expr): _span(span), type(std::move(type)), expr(std::move(expr)) {}
void ASTNS::CastExpr::ast_accept(ASTNS::ExprVisitor &v) { v.ast_visit(*this); }
Span const &ASTNS::CastExpr::span() const { return _span; }
ASTNS::UnaryExpr::UnaryExpr(Span const &span, Located<UnaryOperator> op, std::unique_ptr<Expr> expr): _span(span), op(std::move(op)), expr(std::move(expr)) {}
void ASTNS::UnaryExpr::ast_accept(ASTNS::ExprVisitor &v) { v.ast_visit(*this); }
Span const &ASTNS::UnaryExpr::span() const { return _span; }
ASTNS::AddrofExpr::AddrofExpr(Span const &span, Located<Token> op, std::unique_ptr<Expr> expr, bool mut): _span(span), op(std::move(op)), expr(std::move(expr)), mut(std::move(mut)) {}
void ASTNS::AddrofExpr::ast_accept(ASTNS::ExprVisitor &v) { v.ast_visit(*this); }
Span const &ASTNS::AddrofExpr::span() const { return _span; }
ASTNS::DerefExpr::DerefExpr(Span const &span, Located<Token> op, std::unique_ptr<Expr> expr): _span(span), op(std::move(op)), expr(std::move(expr)) {}
void ASTNS::DerefExpr::ast_accept(ASTNS::ExprVisitor &v) { v.ast_visit(*this); }
Span const &ASTNS::DerefExpr::span() const { return _span; }
ASTNS::CallExpr::CallExpr(Span const &span, std::unique_ptr<Expr> callee, Located<Token> oparn, std::vector<std::unique_ptr<Expr>> args): _span(span), callee(std::move(callee)), oparn(std::move(oparn)), args(std::move(args)) {}
void ASTNS::CallExpr::ast_accept(ASTNS::ExprVisitor &v) { v.ast_visit(*this); }
Span const &ASTNS::CallExpr::span() const { return _span; }
ASTNS::FieldAccessExpr::FieldAccessExpr(Span const &span, std::unique_ptr<Expr> operand, Located<Token> dot, Located<Token> field): _span(span), operand(std::move(operand)), dot(std::move(dot)), field(std::move(field)) {}
void ASTNS::FieldAccessExpr::ast_accept(ASTNS::ExprVisitor &v) { v.ast_visit(*this); }
Span const &ASTNS::FieldAccessExpr::span() const { return _span; }
ASTNS::MethodCallExpr::MethodCallExpr(Span const &span, std::unique_ptr<Expr> operand, Located<Token> dot, Located<Token> method, Located<Token> oparn, std::vector<std::unique_ptr<Expr>> args): _span(span), operand(std::move(operand)), dot(std::move(dot)), method(std::move(method)), oparn(std::move(oparn)), args(std::move(args)) {}
void ASTNS::MethodCallExpr::ast_accept(ASTNS::ExprVisitor &v) { v.ast_visit(*this); }
Span const &ASTNS::MethodCallExpr::span() const { return _span; }
ASTNS::BoolLit::BoolLit(Span const &span, Located<Token> val): _span(span), val(std::move(val)) {}
void ASTNS::BoolLit::ast_accept(ASTNS::ExprVisitor &v) { v.ast_visit(*this); }
Span const &ASTNS::BoolLit::span() const { return _span; }
ASTNS::FloatLit::FloatLit(Span const &span, Located<Token> val): _span(span), val(std::move(val)) {}
void ASTNS::FloatLit::ast_accept(ASTNS::ExprVisitor &v) { v.ast_visit(*this); }
Span const &ASTNS::FloatLit::span() const { return _span; }
ASTNS::IntLit::IntLit(Span const &span, Located<Token> val): _span(span), val(std::move(val)) {}
void ASTNS::IntLit::ast_accept(ASTNS::ExprVisitor &v) { v.ast_visit(*this); }
Span const &ASTNS::IntLit::span() const { return _span; }
ASTNS::CharLit::CharLit(Span const &span, Located<Token> val): _span(span), val(std::move(val)) {}
void ASTNS::CharLit::ast_accept(ASTNS::ExprVisitor &v) { v.ast_visit(*this); }
Span const &ASTNS::CharLit::span() const { return _span; }
ASTNS::StringLit::StringLit(Span const &span, Located<Token> val): _span(span), val(std::move(val)) {}
void ASTNS::StringLit::ast_accept(ASTNS::ExprVisitor &v) { v.ast_visit(*this); }
Span const &ASTNS::StringLit::span() const { return _span; }
ASTNS::ThisExpr::ThisExpr(Span const &span, Located<Token> tok): _span(span), tok(std::move(tok)) {}
void ASTNS::ThisExpr::ast_accept(ASTNS::ExprVisitor &v) { v.ast_visit(*this); }
Span const &ASTNS::ThisExpr::span() const { return _span; }
ASTNS::PathExpr::PathExpr(Span const &span, std::unique_ptr<Path> path): _span(span), path(std::move(path)) {}
void ASTNS::PathExpr::ast_accept(ASTNS::ExprVisitor &v) { v.ast_visit(*this); }
Span const &ASTNS::PathExpr::span() const { return _span; }
ASTNS::Path::Path(Span const &span, std::vector<Located<Token>> segments): _span(span), segments(std::move(segments)) {}
void ASTNS::Path::ast_accept(ASTNS::PathBVisitor &v) { v.ast_visit(*this); }
Span const &ASTNS::Path::span() const { return _span; }
// ASTCPP END
