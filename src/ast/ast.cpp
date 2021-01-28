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
ASTNS::DeclList::DeclList(std::vector<std::unique_ptr<Decl>> decls): decls(std::move(decls)) {}
void ASTNS::DeclList::accept(ASTNS::ListBVisitor &v) { v.visit(*this); }
ASTNS::StmtList::StmtList(std::vector<std::unique_ptr<Stmt>> stmts): stmts(std::move(stmts)) {}
void ASTNS::StmtList::accept(ASTNS::ListBVisitor &v) { v.visit(*this); }
ASTNS::ParamList::ParamList(std::vector<std::unique_ptr<ParamB>> params): params(std::move(params)) {}
void ASTNS::ParamList::accept(ASTNS::ListBVisitor &v) { v.visit(*this); }
ASTNS::ArgList::ArgList(std::vector<std::unique_ptr<Arg>> args): args(std::move(args)) {}
void ASTNS::ArgList::accept(ASTNS::ListBVisitor &v) { v.visit(*this); }
ASTNS::VarStmtItemList::VarStmtItemList(std::vector<std::unique_ptr<VarStmtItem>> items): items(std::move(items)) {}
void ASTNS::VarStmtItemList::accept(ASTNS::ListBVisitor &v) { v.visit(*this); }
ASTNS::ImplMemberList::ImplMemberList(std::vector<std::unique_ptr<ImplMember>> members): members(std::move(members)) {}
void ASTNS::ImplMemberList::accept(ASTNS::ListBVisitor &v) { v.visit(*this); }
ASTNS::PureLocationB::PureLocationB(File const &file): AST(file) {}
ASTNS::PureLocation::PureLocation(int dummy): dummy(std::move(dummy)) {}
void ASTNS::PureLocation::accept(ASTNS::PureLocationBVisitor &v) { v.visit(*this); }
ASTNS::ImplicitDecl::ImplicitDecl(int dummy): dummy(std::move(dummy)) {}
void ASTNS::ImplicitDecl::accept(ASTNS::DeclVisitor &v) { v.visit(*this); }
ASTNS::CU::CU(std::vector<std::unique_ptr<Decl>> decls): decls(std::move(decls)) {}
void ASTNS::CU::accept(ASTNS::CUBVisitor &v) { v.visit(*this); }
ASTNS::ImplDecl::ImplDecl(std::unique_ptr<Type> impl_for, std::vector<std::unique_ptr<ImplMember>> members): impl_for(std::move(impl_for)), members(std::move(members)) {}
void ASTNS::ImplDecl::accept(ASTNS::DeclVisitor &v) { v.visit(*this); }
ASTNS::FunctionDecl::FunctionDecl(std::unique_ptr<Type> retty, Located<Tokens::Identifier> name, std::vector<std::unique_ptr<ParamB>> params, std::unique_ptr<Block> body): retty(std::move(retty)), name(std::move(name)), params(std::move(params)), body(std::move(body)) {}
void ASTNS::FunctionDecl::accept(ASTNS::DeclVisitor &v) { v.visit(*this); }
ASTNS::FunctionImplMember::FunctionImplMember(std::unique_ptr<FunctionDecl> fun): fun(std::move(fun)) {}
void ASTNS::FunctionImplMember::accept(ASTNS::ImplMemberVisitor &v) { v.visit(*this); }
ASTNS::VarStmt::VarStmt(std::vector<std::unique_ptr<VarStmtItem>> items): items(std::move(items)) {}
void ASTNS::VarStmt::accept(ASTNS::StmtVisitor &v) { v.visit(*this); }
ASTNS::VarStmtItem::VarStmtItem(std::unique_ptr<Type> type, bool mut, Located<Tokens::Identifier> name, Located<Tokens::Equal> equal, std::unique_ptr<Expr> expr): type(std::move(type)), mut(std::move(mut)), name(std::move(name)), equal(std::move(equal)), expr(std::move(expr)) {}
void ASTNS::VarStmtItem::accept(ASTNS::VStmtIBVisitor &v) { v.visit(*this); }
ASTNS::ExprStmt::ExprStmt(std::unique_ptr<Expr> expr, bool suppress, Maybe<Span const> dot): expr(std::move(expr)), suppress(std::move(suppress)), dot(std::move(dot)) {}
void ASTNS::ExprStmt::accept(ASTNS::StmtVisitor &v) { v.visit(*this); }
ASTNS::RetStmt::RetStmt(std::unique_ptr<Expr> expr): expr(std::move(expr)) {}
void ASTNS::RetStmt::accept(ASTNS::StmtVisitor &v) { v.visit(*this); }
ASTNS::PathType::PathType(std::unique_ptr<Path> path): path(std::move(path)) {}
void ASTNS::PathType::accept(ASTNS::TypeVisitor &v) { v.visit(*this); }
ASTNS::PointerType::PointerType(bool mut, std::unique_ptr<Type> type): mut(std::move(mut)), type(std::move(type)) {}
void ASTNS::PointerType::accept(ASTNS::TypeVisitor &v) { v.visit(*this); }
ASTNS::ThisType::ThisType(Located<Tokens::This> th): th(std::move(th)) {}
void ASTNS::ThisType::accept(ASTNS::TypeVisitor &v) { v.visit(*this); }
ASTNS::Arg::Arg(std::unique_ptr<Expr> expr): expr(std::move(expr)) {}
void ASTNS::Arg::accept(ASTNS::ArgBVisitor &v) { v.visit(*this); }
ASTNS::Param::Param(std::unique_ptr<Type> type, Located<Tokens::Identifier> name, bool mut): type(std::move(type)), name(std::move(name)), mut(std::move(mut)) {}
void ASTNS::Param::accept(ASTNS::ParamBVisitor &v) { v.visit(*this); }
ASTNS::ThisParam::ThisParam(bool ptr, bool mut): ptr(std::move(ptr)), mut(std::move(mut)) {}
void ASTNS::ThisParam::accept(ASTNS::ParamBVisitor &v) { v.visit(*this); }
ASTNS::Block::Block(std::vector<std::unique_ptr<Stmt>> stmts): stmts(std::move(stmts)) {}
void ASTNS::Block::accept(ASTNS::ExprVisitor &v) { v.visit(*this); }
ASTNS::IfExpr::IfExpr(Located<Tokens::If> iftok, Maybe<Located<Tokens::Else>> elsetok, std::unique_ptr<Expr> cond, std::unique_ptr<Expr> trues, std::unique_ptr<Expr> falses): iftok(std::move(iftok)), elsetok(std::move(elsetok)), cond(std::move(cond)), trues(std::move(trues)), falses(std::move(falses)) {}
void ASTNS::IfExpr::accept(ASTNS::ExprVisitor &v) { v.visit(*this); }
ASTNS::WhileExpr::WhileExpr(std::unique_ptr<Expr> cond, std::unique_ptr<Expr> body): cond(std::move(cond)), body(std::move(body)) {}
void ASTNS::WhileExpr::accept(ASTNS::ExprVisitor &v) { v.visit(*this); }
ASTNS::AssignmentExpr::AssignmentExpr(std::unique_ptr<Expr> target, Located<AssignOperator> equal, std::unique_ptr<Expr> expr): target(std::move(target)), equal(std::move(equal)), expr(std::move(expr)) {}
void ASTNS::AssignmentExpr::accept(ASTNS::ExprVisitor &v) { v.visit(*this); }
ASTNS::ShortCircuitExpr::ShortCircuitExpr(std::unique_ptr<Expr> lhs, Located<ShortCircuitOperator> op, std::unique_ptr<Expr> rhs): lhs(std::move(lhs)), op(std::move(op)), rhs(std::move(rhs)) {}
void ASTNS::ShortCircuitExpr::accept(ASTNS::ExprVisitor &v) { v.visit(*this); }
ASTNS::BinaryExpr::BinaryExpr(std::unique_ptr<Expr> lhs, Located<BinaryOperator> op, std::unique_ptr<Expr> rhs): lhs(std::move(lhs)), op(std::move(op)), rhs(std::move(rhs)) {}
void ASTNS::BinaryExpr::accept(ASTNS::ExprVisitor &v) { v.visit(*this); }
ASTNS::CastExpr::CastExpr(std::unique_ptr<Type> type, std::unique_ptr<Expr> expr): type(std::move(type)), expr(std::move(expr)) {}
void ASTNS::CastExpr::accept(ASTNS::ExprVisitor &v) { v.visit(*this); }
ASTNS::UnaryExpr::UnaryExpr(Located<UnaryOperator> op, std::unique_ptr<Expr> expr): op(std::move(op)), expr(std::move(expr)) {}
void ASTNS::UnaryExpr::accept(ASTNS::ExprVisitor &v) { v.visit(*this); }
ASTNS::AddrofExpr::AddrofExpr(Located<Tokens::Amper> op, std::unique_ptr<Expr> expr, bool mut): op(std::move(op)), expr(std::move(expr)), mut(std::move(mut)) {}
void ASTNS::AddrofExpr::accept(ASTNS::ExprVisitor &v) { v.visit(*this); }
ASTNS::DerefExpr::DerefExpr(Located<Tokens::Star> op, std::unique_ptr<Expr> expr): op(std::move(op)), expr(std::move(expr)) {}
void ASTNS::DerefExpr::accept(ASTNS::ExprVisitor &v) { v.visit(*this); }
ASTNS::CallExpr::CallExpr(std::unique_ptr<Expr> callee, Located<Tokens::OParen> oparn, std::vector<std::unique_ptr<Arg>> args): callee(std::move(callee)), oparn(std::move(oparn)), args(std::move(args)) {}
void ASTNS::CallExpr::accept(ASTNS::ExprVisitor &v) { v.visit(*this); }
ASTNS::FieldAccessExpr::FieldAccessExpr(std::unique_ptr<Expr> operand, Located<Tokens::Period> dot, Located<Tokens::Identifier> field): operand(std::move(operand)), dot(std::move(dot)), field(std::move(field)) {}
void ASTNS::FieldAccessExpr::accept(ASTNS::ExprVisitor &v) { v.visit(*this); }
ASTNS::MethodCallExpr::MethodCallExpr(std::unique_ptr<Expr> operand, Located<Tokens::Period> dot, Located<Tokens::Identifier> method, Located<Tokens::OParen> oparn, std::vector<std::unique_ptr<Arg>> args): operand(std::move(operand)), dot(std::move(dot)), method(std::move(method)), oparn(std::move(oparn)), args(std::move(args)) {}
void ASTNS::MethodCallExpr::accept(ASTNS::ExprVisitor &v) { v.visit(*this); }
ASTNS::BoolLit::BoolLit(Located<bool> val): val(std::move(val)) {}
void ASTNS::BoolLit::accept(ASTNS::ExprVisitor &v) { v.visit(*this); }
ASTNS::FloatLit::FloatLit(Located<double> val): val(std::move(val)) {}
void ASTNS::FloatLit::accept(ASTNS::ExprVisitor &v) { v.visit(*this); }
ASTNS::IntLit::IntLit(Located<uint64_t> val): val(std::move(val)) {}
void ASTNS::IntLit::accept(ASTNS::ExprVisitor &v) { v.visit(*this); }
ASTNS::CharLit::CharLit(Located<char> val): val(std::move(val)) {}
void ASTNS::CharLit::accept(ASTNS::ExprVisitor &v) { v.visit(*this); }
ASTNS::StringLit::StringLit(Located<std::string> val): val(std::move(val)) {}
void ASTNS::StringLit::accept(ASTNS::ExprVisitor &v) { v.visit(*this); }
ASTNS::ThisExpr::ThisExpr(Span tok): tok(std::move(tok)) {}
void ASTNS::ThisExpr::accept(ASTNS::ExprVisitor &v) { v.visit(*this); }
ASTNS::PathExpr::PathExpr(std::unique_ptr<Path> path): path(std::move(path)) {}
void ASTNS::PathExpr::accept(ASTNS::ExprVisitor &v) { v.visit(*this); }
ASTNS::Path::Path(std::vector<Located<Tokens::Identifier>> segments): segments(std::move(segments)) {}
void ASTNS::Path::accept(ASTNS::PathBVisitor &v) { v.visit(*this); }
// ASTCPP END
