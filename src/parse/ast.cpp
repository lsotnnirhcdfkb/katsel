// ASTCPP START

// The following code was autogenerated - see the utils/ directory
#include "parse/ast.h"
ASTNS::Additionexpr::Additionexpr(std::unique_ptr<AST> lhs, Token op, std::unique_ptr<AST> rhs): lhs(std::move(lhs)), op(op), rhs(std::move(rhs)), form(ASTNS::Additionexpr::Form::LHS_OP_RHS) {}
void ASTNS::Additionexpr::accept(ASTVisitor *v) { v->visitAdditionexpr(this); }
ASTNS::Args::Args(std::unique_ptr<AST> args, Token comma, std::unique_ptr<AST> expr): args(std::move(args)), comma(comma), expr(std::move(expr)), form(ASTNS::Args::Form::ARGS_COMMA_EXPR) {}
ASTNS::Args::Args(std::unique_ptr<AST> expr): expr(std::move(expr)), form(ASTNS::Args::Form::EXPR) {}
void ASTNS::Args::accept(ASTVisitor *v) { v->visitArgs(this); }
ASTNS::Assignmentexpr::Assignmentexpr(std::unique_ptr<AST> target, Token equal, std::unique_ptr<AST> value): target(std::move(target)), equal(equal), value(std::move(value)), form(ASTNS::Assignmentexpr::Form::TARGET_EQUAL_VALUE) {}
void ASTNS::Assignmentexpr::accept(ASTVisitor *v) { v->visitAssignmentexpr(this); }
ASTNS::Binandexpr::Binandexpr(std::unique_ptr<AST> lhs, Token op, std::unique_ptr<AST> rhs): lhs(std::move(lhs)), op(op), rhs(std::move(rhs)), form(ASTNS::Binandexpr::Form::LHS_OP_RHS) {}
void ASTNS::Binandexpr::accept(ASTVisitor *v) { v->visitBinandexpr(this); }
ASTNS::Binnotexpr::Binnotexpr(Token op, std::unique_ptr<AST> operand): op(op), operand(std::move(operand)), form(ASTNS::Binnotexpr::Form::OP_OPERAND) {}
void ASTNS::Binnotexpr::accept(ASTVisitor *v) { v->visitBinnotexpr(this); }
ASTNS::Binorexpr::Binorexpr(std::unique_ptr<AST> lhs, Token op, std::unique_ptr<AST> rhs): lhs(std::move(lhs)), op(op), rhs(std::move(rhs)), form(ASTNS::Binorexpr::Form::LHS_OP_RHS) {}
void ASTNS::Binorexpr::accept(ASTVisitor *v) { v->visitBinorexpr(this); }
ASTNS::Bitandexpr::Bitandexpr(std::unique_ptr<AST> lhs, Token op, std::unique_ptr<AST> rhs): lhs(std::move(lhs)), op(op), rhs(std::move(rhs)), form(ASTNS::Bitandexpr::Form::LHS_OP_RHS) {}
void ASTNS::Bitandexpr::accept(ASTVisitor *v) { v->visitBitandexpr(this); }
ASTNS::Bitorexpr::Bitorexpr(std::unique_ptr<AST> lhs, Token op, std::unique_ptr<AST> rhs): lhs(std::move(lhs)), op(op), rhs(std::move(rhs)), form(ASTNS::Bitorexpr::Form::LHS_OP_RHS) {}
void ASTNS::Bitorexpr::accept(ASTVisitor *v) { v->visitBitorexpr(this); }
ASTNS::Bitshiftexpr::Bitshiftexpr(std::unique_ptr<AST> lhs, Token op, std::unique_ptr<AST> rhs): lhs(std::move(lhs)), op(op), rhs(std::move(rhs)), form(ASTNS::Bitshiftexpr::Form::LHS_OP_RHS) {}
void ASTNS::Bitshiftexpr::accept(ASTVisitor *v) { v->visitBitshiftexpr(this); }
ASTNS::Bitxorexpr::Bitxorexpr(std::unique_ptr<AST> lhs, Token op, std::unique_ptr<AST> rhs): lhs(std::move(lhs)), op(op), rhs(std::move(rhs)), form(ASTNS::Bitxorexpr::Form::LHS_OP_RHS) {}
void ASTNS::Bitxorexpr::accept(ASTVisitor *v) { v->visitBitxorexpr(this); }
ASTNS::Block::Block(Token ocurb, std::unique_ptr<AST> stmts, Token ccurb): ocurb(ocurb), stmts(std::move(stmts)), ccurb(ccurb), form(ASTNS::Block::Form::OCURB_STMTS_CCURB) {}
void ASTNS::Block::accept(ASTVisitor *v) { v->visitBlock(this); }
ASTNS::Call::Call(std::unique_ptr<AST> callee, Token oparn, std::unique_ptr<AST> args, Token cparn): callee(std::move(callee)), oparn(oparn), args(std::move(args)), cparn(cparn), form(ASTNS::Call::Form::CALLEE_OPARN_ARGS_CPARN) {}
ASTNS::Call::Call(std::unique_ptr<AST> callee, Token oparn, Token cparn): callee(std::move(callee)), oparn(oparn), cparn(cparn), form(ASTNS::Call::Form::CALLEE_OPARN_CPARN) {}
void ASTNS::Call::accept(ASTVisitor *v) { v->visitCall(this); }
ASTNS::Compeqexpr::Compeqexpr(std::unique_ptr<AST> lhs, Token op, std::unique_ptr<AST> rhs): lhs(std::move(lhs)), op(op), rhs(std::move(rhs)), form(ASTNS::Compeqexpr::Form::LHS_OP_RHS) {}
void ASTNS::Compeqexpr::accept(ASTVisitor *v) { v->visitCompeqexpr(this); }
ASTNS::Complgtexpr::Complgtexpr(std::unique_ptr<AST> lhs, Token op, std::unique_ptr<AST> rhs): lhs(std::move(lhs)), op(op), rhs(std::move(rhs)), form(ASTNS::Complgtexpr::Form::LHS_OP_RHS) {}
void ASTNS::Complgtexpr::accept(ASTVisitor *v) { v->visitComplgtexpr(this); }
void ASTNS::Decl::accept(ASTVisitor *v) { v->visitDecl(this); }
ASTNS::Declarations::Declarations(std::unique_ptr<AST> decls, std::unique_ptr<AST> decl): decls(std::move(decls)), decl(std::move(decl)), form(ASTNS::Declarations::Form::DECLS_DECL) {}
ASTNS::Declarations::Declarations(std::unique_ptr<AST> decl): decl(std::move(decl)), form(ASTNS::Declarations::Form::DECL) {}
void ASTNS::Declarations::accept(ASTVisitor *v) { v->visitDeclarations(this); }
void ASTNS::Expression::accept(ASTVisitor *v) { v->visitExpression(this); }
ASTNS::Exprstmt::Exprstmt(std::unique_ptr<AST> expr, Token semi): expr(std::move(expr)), semi(semi), form(ASTNS::Exprstmt::Form::EXPR_SEMI) {}
void ASTNS::Exprstmt::accept(ASTVisitor *v) { v->visitExprstmt(this); }
ASTNS::Function::Function(std::unique_ptr<AST> retty, Token name, Token oparn, Token cparn, std::unique_ptr<AST> body): retty(std::move(retty)), name(name), oparn(oparn), cparn(cparn), body(std::move(body)), form(ASTNS::Function::Form::RETTY_NAME_OPARN_CPARN_BODY) {}
ASTNS::Function::Function(std::unique_ptr<AST> retty, Token name, Token oparn, std::unique_ptr<AST> paramlist, Token cparn, std::unique_ptr<AST> body): retty(std::move(retty)), name(name), oparn(oparn), paramlist(std::move(paramlist)), cparn(cparn), body(std::move(body)), form(ASTNS::Function::Form::RETTY_NAME_OPARN_PARAMLIST_CPARN_BODY) {}
void ASTNS::Function::accept(ASTVisitor *v) { v->visitFunction(this); }
ASTNS::Multexpr::Multexpr(std::unique_ptr<AST> lhs, Token op, std::unique_ptr<AST> rhs): lhs(std::move(lhs)), op(op), rhs(std::move(rhs)), form(ASTNS::Multexpr::Form::LHS_OP_RHS) {}
void ASTNS::Multexpr::accept(ASTVisitor *v) { v->visitMultexpr(this); }
ASTNS::Paramlist::Paramlist(std::unique_ptr<AST> plist, Token comma, std::unique_ptr<AST> type, Token name): plist(std::move(plist)), comma(comma), type(std::move(type)), name(name), form(ASTNS::Paramlist::Form::PLIST_COMMA_TYPE_NAME) {}
ASTNS::Paramlist::Paramlist(std::unique_ptr<AST> type, Token name): type(std::move(type)), name(name), form(ASTNS::Paramlist::Form::TYPE_NAME) {}
void ASTNS::Paramlist::accept(ASTVisitor *v) { v->visitParamlist(this); }
ASTNS::Primary::Primary(Token value): value(value), form(ASTNS::Primary::Form::VALUE) {}
ASTNS::Primary::Primary(Token oparn, std::unique_ptr<AST> expr, Token cparn): oparn(oparn), expr(std::move(expr)), cparn(cparn), form(ASTNS::Primary::Form::OPARN_EXPR_CPARN) {}
void ASTNS::Primary::accept(ASTVisitor *v) { v->visitPrimary(this); }
ASTNS::Retstmt::Retstmt(Token ret, std::unique_ptr<AST> expr, Token semi): ret(ret), expr(std::move(expr)), semi(semi), form(ASTNS::Retstmt::Form::RET_EXPR_SEMI) {}
void ASTNS::Retstmt::accept(ASTVisitor *v) { v->visitRetstmt(this); }
void ASTNS::Stmt::accept(ASTVisitor *v) { v->visitStmt(this); }
ASTNS::Stmts::Stmts(std::unique_ptr<AST> stmts, std::unique_ptr<AST> stmt): stmts(std::move(stmts)), stmt(std::move(stmt)), form(ASTNS::Stmts::Form::STMTS_STMT) {}
void ASTNS::Stmts::accept(ASTVisitor *v) { v->visitStmts(this); }
ASTNS::Ternaryexpr::Ternaryexpr(std::unique_ptr<AST> cond, Token quest, std::unique_ptr<AST> trues, Token colon, std::unique_ptr<AST> falses): cond(std::move(cond)), quest(quest), trues(std::move(trues)), colon(colon), falses(std::move(falses)), form(ASTNS::Ternaryexpr::Form::COND_QUEST_TRUES_COLON_FALSES) {}
void ASTNS::Ternaryexpr::accept(ASTVisitor *v) { v->visitTernaryexpr(this); }
ASTNS::Type::Type(Token type): type(type), form(ASTNS::Type::Form::TYPE) {}
void ASTNS::Type::accept(ASTVisitor *v) { v->visitType(this); }
ASTNS::Unary::Unary(Token op, std::unique_ptr<AST> operand): op(op), operand(std::move(operand)), form(ASTNS::Unary::Form::OP_OPERAND) {}
void ASTNS::Unary::accept(ASTVisitor *v) { v->visitUnary(this); }
ASTNS::Varstmt::Varstmt(Token var, std::unique_ptr<AST> type, std::unique_ptr<AST> assignments, Token semi): var(var), type(std::move(type)), assignments(std::move(assignments)), semi(semi), form(ASTNS::Varstmt::Form::VAR_TYPE_ASSIGNMENTS_SEMI) {}
void ASTNS::Varstmt::accept(ASTVisitor *v) { v->visitVarstmt(this); }
ASTNS::Varstmtfinisher::Varstmtfinisher(std::unique_ptr<AST> assignments, Token comma, Token name, Token equal, std::unique_ptr<AST> expr): assignments(std::move(assignments)), comma(comma), name(name), equal(equal), expr(std::move(expr)), form(ASTNS::Varstmtfinisher::Form::ASSIGNMENTS_COMMA_NAME_EQUAL_EXPR) {}
ASTNS::Varstmtfinisher::Varstmtfinisher(Token name, Token equal, std::unique_ptr<AST> expr): name(name), equal(equal), expr(std::move(expr)), form(ASTNS::Varstmtfinisher::Form::NAME_EQUAL_EXPR) {}
void ASTNS::Varstmtfinisher::accept(ASTVisitor *v) { v->visitVarstmtfinisher(this); }
// This code was autogenerated - see the utils/ directory

// ASTCPP END
