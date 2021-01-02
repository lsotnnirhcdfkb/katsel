#include "ast/ast.h"
// ASTCPP START
// The following code was autogenerated - see the utils/ directory
ASTNS::AST::AST(File const &file): file(file) {}
ASTNS::CUB::CUB(File const &file): AST(file) {}
ASTNS::Decl::Decl(File const &file): AST(file) {}
ASTNS::Stmt::Stmt(File const &file): AST(file) {}
ASTNS::Expr::Expr(File const &file): AST(file) {}
ASTNS::Type::Type(File const &file): AST(file) {}
ASTNS::ArgB::ArgB(File const &file): AST(file) {}
ASTNS::ParamB::ParamB(File const &file): AST(file) {}
ASTNS::VStmtIB::VStmtIB(File const &file): AST(file) {}
ASTNS::ImplRetB::ImplRetB(File const &file): AST(file) {}
ASTNS::PureLocationB::PureLocationB(File const &file): AST(file) {}
ASTNS::PureLocation::PureLocation(File const &file, Location start, Location end, int dummy): PureLocationB(file), _start(start), _end(end), dummy(std::move(dummy)) {}
void ASTNS::PureLocation::accept(ASTNS::PureLocationB::Visitor *v) { v->visitPureLocation(this); }
Location const & ASTNS::PureLocation::start() { return _start; }
Location const & ASTNS::PureLocation::end() { return _end; }
ASTNS::CU::CU(File const &file, Location start, Location end, std::unique_ptr<DeclList> decls): CUB(file), _start(start), _end(end), decls(std::move(decls)) {}
void ASTNS::CU::accept(ASTNS::CUB::Visitor *v) { v->visitCU(this); }
Location const & ASTNS::CU::start() { return _start; }
Location const & ASTNS::CU::end() { return _end; }
ASTNS::DeclList::DeclList(File const &file, Location start, Location end, std::vector<std::unique_ptr<Decl>> decls): Decl(file), _start(start), _end(end), decls(std::move(decls)) {}
void ASTNS::DeclList::accept(ASTNS::Decl::Visitor *v) { v->visitDeclList(this); }
Location const & ASTNS::DeclList::start() { return _start; }
Location const & ASTNS::DeclList::end() { return _end; }
ASTNS::ImplDecl::ImplDecl(File const &file, Location start, Location end, std::unique_ptr<Type> implfor): Decl(file), _start(start), _end(end), implfor(std::move(implfor)) {}
void ASTNS::ImplDecl::accept(ASTNS::Decl::Visitor *v) { v->visitImplDecl(this); }
Location const & ASTNS::ImplDecl::start() { return _start; }
Location const & ASTNS::ImplDecl::end() { return _end; }
ASTNS::FunctionDecl::FunctionDecl(File const &file, Location start, Location end, std::unique_ptr<Type> retty, Token name, std::unique_ptr<ParamList> params, std::unique_ptr<Block> body): Decl(file), _start(start), _end(end), retty(std::move(retty)), name(std::move(name)), params(std::move(params)), body(std::move(body)) {}
void ASTNS::FunctionDecl::accept(ASTNS::Decl::Visitor *v) { v->visitFunctionDecl(this); }
Location const & ASTNS::FunctionDecl::start() { return _start; }
Location const & ASTNS::FunctionDecl::end() { return _end; }
ASTNS::VarStmt::VarStmt(File const &file, Location start, Location end, std::unique_ptr<VarStmtItemList> assignments): Stmt(file), _start(start), _end(end), assignments(std::move(assignments)) {}
void ASTNS::VarStmt::accept(ASTNS::Stmt::Visitor *v) { v->visitVarStmt(this); }
Location const & ASTNS::VarStmt::start() { return _start; }
Location const & ASTNS::VarStmt::end() { return _end; }
ASTNS::VarStmtItem::VarStmtItem(File const &file, Location start, Location end, std::unique_ptr<Type> type, Token name, Token equal, std::unique_ptr<Expr> expr): VStmtIB(file), _start(start), _end(end), type(std::move(type)), name(std::move(name)), equal(std::move(equal)), expr(std::move(expr)) {}
void ASTNS::VarStmtItem::accept(ASTNS::VStmtIB::Visitor *v) { v->visitVarStmtItem(this); }
Location const & ASTNS::VarStmtItem::start() { return _start; }
Location const & ASTNS::VarStmtItem::end() { return _end; }
ASTNS::VarStmtItemList::VarStmtItemList(File const &file, Location start, Location end, std::vector<std::unique_ptr<VarStmtItem>> items): VStmtIB(file), _start(start), _end(end), items(std::move(items)) {}
void ASTNS::VarStmtItemList::accept(ASTNS::VStmtIB::Visitor *v) { v->visitVarStmtItemList(this); }
Location const & ASTNS::VarStmtItemList::start() { return _start; }
Location const & ASTNS::VarStmtItemList::end() { return _end; }
ASTNS::ExprStmt::ExprStmt(File const &file, Location start, Location end, std::unique_ptr<Expr> expr): Stmt(file), _start(start), _end(end), expr(std::move(expr)) {}
void ASTNS::ExprStmt::accept(ASTNS::Stmt::Visitor *v) { v->visitExprStmt(this); }
Location const & ASTNS::ExprStmt::start() { return _start; }
Location const & ASTNS::ExprStmt::end() { return _end; }
ASTNS::RetStmt::RetStmt(File const &file, Location start, Location end, std::unique_ptr<Expr> expr): Stmt(file), _start(start), _end(end), expr(std::move(expr)) {}
void ASTNS::RetStmt::accept(ASTNS::Stmt::Visitor *v) { v->visitRetStmt(this); }
Location const & ASTNS::RetStmt::start() { return _start; }
Location const & ASTNS::RetStmt::end() { return _end; }
ASTNS::StmtList::StmtList(File const &file, Location start, Location end, std::vector<std::unique_ptr<Stmt>> stmts): Stmt(file), _start(start), _end(end), stmts(std::move(stmts)) {}
void ASTNS::StmtList::accept(ASTNS::Stmt::Visitor *v) { v->visitStmtList(this); }
Location const & ASTNS::StmtList::start() { return _start; }
Location const & ASTNS::StmtList::end() { return _end; }
ASTNS::ImplRet::ImplRet(File const &file, Location start, Location end, std::unique_ptr<Expr> expr): ImplRetB(file), _start(start), _end(end), expr(std::move(expr)) {}
void ASTNS::ImplRet::accept(ASTNS::ImplRetB::Visitor *v) { v->visitImplRet(this); }
Location const & ASTNS::ImplRet::start() { return _start; }
Location const & ASTNS::ImplRet::end() { return _end; }
ASTNS::PrimitiveType::PrimitiveType(File const &file, Location start, Location end, Token ty): Type(file), _start(start), _end(end), ty(std::move(ty)) {}
void ASTNS::PrimitiveType::accept(ASTNS::Type::Visitor *v) { v->visitPrimitiveType(this); }
Location const & ASTNS::PrimitiveType::start() { return _start; }
Location const & ASTNS::PrimitiveType::end() { return _end; }
ASTNS::PointerType::PointerType(File const &file, Location start, Location end, std::unique_ptr<Type> type): Type(file), _start(start), _end(end), type(std::move(type)) {}
void ASTNS::PointerType::accept(ASTNS::Type::Visitor *v) { v->visitPointerType(this); }
Location const & ASTNS::PointerType::start() { return _start; }
Location const & ASTNS::PointerType::end() { return _end; }
ASTNS::Arg::Arg(File const &file, Location start, Location end, std::unique_ptr<Expr> expr): ArgB(file), _start(start), _end(end), expr(std::move(expr)) {}
void ASTNS::Arg::accept(ASTNS::ArgB::Visitor *v) { v->visitArg(this); }
Location const & ASTNS::Arg::start() { return _start; }
Location const & ASTNS::Arg::end() { return _end; }
ASTNS::ArgList::ArgList(File const &file, Location start, Location end, std::vector<std::unique_ptr<Arg>> args): ArgB(file), _start(start), _end(end), args(std::move(args)) {}
void ASTNS::ArgList::accept(ASTNS::ArgB::Visitor *v) { v->visitArgList(this); }
Location const & ASTNS::ArgList::start() { return _start; }
Location const & ASTNS::ArgList::end() { return _end; }
ASTNS::Param::Param(File const &file, Location start, Location end, std::unique_ptr<Type> type, Token name): ParamB(file), _start(start), _end(end), type(std::move(type)), name(std::move(name)) {}
void ASTNS::Param::accept(ASTNS::ParamB::Visitor *v) { v->visitParam(this); }
Location const & ASTNS::Param::start() { return _start; }
Location const & ASTNS::Param::end() { return _end; }
ASTNS::ParamList::ParamList(File const &file, Location start, Location end, std::vector<std::unique_ptr<Param>> params): ParamB(file), _start(start), _end(end), params(std::move(params)) {}
void ASTNS::ParamList::accept(ASTNS::ParamB::Visitor *v) { v->visitParamList(this); }
Location const & ASTNS::ParamList::start() { return _start; }
Location const & ASTNS::ParamList::end() { return _end; }
ASTNS::Block::Block(File const &file, Location start, Location end, std::unique_ptr<StmtList> stmts, std::unique_ptr<ImplRet> implRet): Expr(file), _start(start), _end(end), stmts(std::move(stmts)), implRet(std::move(implRet)) {}
void ASTNS::Block::accept(ASTNS::Expr::Visitor *v) { v->visitBlock(this); }
Location const & ASTNS::Block::start() { return _start; }
Location const & ASTNS::Block::end() { return _end; }
ASTNS::IfExpr::IfExpr(File const &file, Location start, Location end, Token iftok, std::unique_ptr<Expr> cond, std::unique_ptr<Expr> trues, std::unique_ptr<Expr> falses): Expr(file), _start(start), _end(end), iftok(std::move(iftok)), cond(std::move(cond)), trues(std::move(trues)), falses(std::move(falses)) {}
void ASTNS::IfExpr::accept(ASTNS::Expr::Visitor *v) { v->visitIfExpr(this); }
Location const & ASTNS::IfExpr::start() { return _start; }
Location const & ASTNS::IfExpr::end() { return _end; }
ASTNS::ForExpr::ForExpr(File const &file, Location start, Location end, std::unique_ptr<VarStmt> initial, std::unique_ptr<Expr> cond, std::unique_ptr<Expr> increment, std::unique_ptr<Expr> body): Expr(file), _start(start), _end(end), initial(std::move(initial)), cond(std::move(cond)), increment(std::move(increment)), body(std::move(body)) {}
void ASTNS::ForExpr::accept(ASTNS::Expr::Visitor *v) { v->visitForExpr(this); }
Location const & ASTNS::ForExpr::start() { return _start; }
Location const & ASTNS::ForExpr::end() { return _end; }
ASTNS::AssignmentExpr::AssignmentExpr(File const &file, Location start, Location end, std::unique_ptr<Expr> target, Token equal, std::unique_ptr<Expr> expr): Expr(file), _start(start), _end(end), target(std::move(target)), equal(std::move(equal)), expr(std::move(expr)) {}
void ASTNS::AssignmentExpr::accept(ASTNS::Expr::Visitor *v) { v->visitAssignmentExpr(this); }
Location const & ASTNS::AssignmentExpr::start() { return _start; }
Location const & ASTNS::AssignmentExpr::end() { return _end; }
ASTNS::ShortCircuitExpr::ShortCircuitExpr(File const &file, Location start, Location end, std::unique_ptr<Expr> lhs, Token op, std::unique_ptr<Expr> rhs): Expr(file), _start(start), _end(end), lhs(std::move(lhs)), op(std::move(op)), rhs(std::move(rhs)) {}
void ASTNS::ShortCircuitExpr::accept(ASTNS::Expr::Visitor *v) { v->visitShortCircuitExpr(this); }
Location const & ASTNS::ShortCircuitExpr::start() { return _start; }
Location const & ASTNS::ShortCircuitExpr::end() { return _end; }
ASTNS::BinaryExpr::BinaryExpr(File const &file, Location start, Location end, std::unique_ptr<Expr> lhs, Token op, std::unique_ptr<Expr> rhs): Expr(file), _start(start), _end(end), lhs(std::move(lhs)), op(std::move(op)), rhs(std::move(rhs)) {}
void ASTNS::BinaryExpr::accept(ASTNS::Expr::Visitor *v) { v->visitBinaryExpr(this); }
Location const & ASTNS::BinaryExpr::start() { return _start; }
Location const & ASTNS::BinaryExpr::end() { return _end; }
ASTNS::CastExpr::CastExpr(File const &file, Location start, Location end, std::unique_ptr<Type> type, std::unique_ptr<Expr> expr): Expr(file), _start(start), _end(end), type(std::move(type)), expr(std::move(expr)) {}
void ASTNS::CastExpr::accept(ASTNS::Expr::Visitor *v) { v->visitCastExpr(this); }
Location const & ASTNS::CastExpr::start() { return _start; }
Location const & ASTNS::CastExpr::end() { return _end; }
ASTNS::UnaryExpr::UnaryExpr(File const &file, Location start, Location end, Token op, std::unique_ptr<Expr> expr): Expr(file), _start(start), _end(end), op(std::move(op)), expr(std::move(expr)) {}
void ASTNS::UnaryExpr::accept(ASTNS::Expr::Visitor *v) { v->visitUnaryExpr(this); }
Location const & ASTNS::UnaryExpr::start() { return _start; }
Location const & ASTNS::UnaryExpr::end() { return _end; }
ASTNS::AddrofExpr::AddrofExpr(File const &file, Location start, Location end, Token op, std::unique_ptr<Expr> expr): Expr(file), _start(start), _end(end), op(std::move(op)), expr(std::move(expr)) {}
void ASTNS::AddrofExpr::accept(ASTNS::Expr::Visitor *v) { v->visitAddrofExpr(this); }
Location const & ASTNS::AddrofExpr::start() { return _start; }
Location const & ASTNS::AddrofExpr::end() { return _end; }
ASTNS::DerefExpr::DerefExpr(File const &file, Location start, Location end, Token op, std::unique_ptr<Expr> expr): Expr(file), _start(start), _end(end), op(std::move(op)), expr(std::move(expr)) {}
void ASTNS::DerefExpr::accept(ASTNS::Expr::Visitor *v) { v->visitDerefExpr(this); }
Location const & ASTNS::DerefExpr::start() { return _start; }
Location const & ASTNS::DerefExpr::end() { return _end; }
ASTNS::CallExpr::CallExpr(File const &file, Location start, Location end, std::unique_ptr<Expr> callee, Token oparn, std::unique_ptr<ArgList> args): Expr(file), _start(start), _end(end), callee(std::move(callee)), oparn(std::move(oparn)), args(std::move(args)) {}
void ASTNS::CallExpr::accept(ASTNS::Expr::Visitor *v) { v->visitCallExpr(this); }
Location const & ASTNS::CallExpr::start() { return _start; }
Location const & ASTNS::CallExpr::end() { return _end; }
ASTNS::PrimaryExpr::PrimaryExpr(File const &file, Location start, Location end, Token value): Expr(file), _start(start), _end(end), value(std::move(value)) {}
void ASTNS::PrimaryExpr::accept(ASTNS::Expr::Visitor *v) { v->visitPrimaryExpr(this); }
Location const & ASTNS::PrimaryExpr::start() { return _start; }
Location const & ASTNS::PrimaryExpr::end() { return _end; }
// This code was autogenerated - see the utils/ directory
// ASTCPP END
