// ASTCPP START

// The following code was autogenerated - see the utils/ directory
#include "ast/ast.h"
ASTNS::AdditionExpr::AdditionExpr(std::unique_ptr<ExprB> lhs, Token op, std::unique_ptr<ExprB> rhs): lhs(std::move(lhs)), op(op), rhs(std::move(rhs)), form(ASTNS::AdditionExpr::Form::ATA) {}
void ASTNS::AdditionExpr::accept(ASTNS::ExprBVisitor *v) { v->visitAdditionExpr(this); }
bool ASTNS::AdditionExpr::empty() { return false; }
ASTNS::Arg::Arg(std::unique_ptr<ExprB> expr): expr(std::move(expr)), form(ASTNS::Arg::Form::A) {}
void ASTNS::Arg::accept(ASTNS::ArgBVisitor *v) { v->visitArg(this); }
bool ASTNS::Arg::empty() { return false; }
ASTNS::ArgList::ArgList(std::unique_ptr<ArgB> argsegment, Token comma): argsegment(std::move(argsegment)), comma(comma), form(ASTNS::ArgList::Form::AT) {}
void ASTNS::ArgList::accept(ASTNS::ArgBVisitor *v) { v->visitArgList(this); }
bool ASTNS::ArgList::empty() { return false; }
ASTNS::ArgList_OPT::ArgList_OPT(): form(ASTNS::ArgList_OPT::Form::EMPTY) {}
void ASTNS::ArgList_OPT::accept(ASTNS::ArgBVisitor *v) { v->visitArgList_OPT(this); }
bool ASTNS::ArgList_OPT::empty() { return form == Form::EMPTY; }
ASTNS::ArgSegment::ArgSegment(std::unique_ptr<ArgB> argsegment, Token comma, std::unique_ptr<ArgB> anotherarg): argsegment(std::move(argsegment)), comma(comma), anotherarg(std::move(anotherarg)), form(ASTNS::ArgSegment::Form::ATA) {}
void ASTNS::ArgSegment::accept(ASTNS::ArgBVisitor *v) { v->visitArgSegment(this); }
bool ASTNS::ArgSegment::empty() { return false; }
ASTNS::AssignmentExpr::AssignmentExpr(std::unique_ptr<ExprB> target, Token equal, std::unique_ptr<ExprB> value): target(std::move(target)), equal(equal), value(std::move(value)), form(ASTNS::AssignmentExpr::Form::ATA) {}
void ASTNS::AssignmentExpr::accept(ASTNS::ExprBVisitor *v) { v->visitAssignmentExpr(this); }
bool ASTNS::AssignmentExpr::empty() { return false; }
ASTNS::BinAndExpr::BinAndExpr(std::unique_ptr<ExprB> lhs, Token op, std::unique_ptr<ExprB> rhs): lhs(std::move(lhs)), op(op), rhs(std::move(rhs)), form(ASTNS::BinAndExpr::Form::ATA) {}
void ASTNS::BinAndExpr::accept(ASTNS::ExprBVisitor *v) { v->visitBinAndExpr(this); }
bool ASTNS::BinAndExpr::empty() { return false; }
ASTNS::BinOrExpr::BinOrExpr(std::unique_ptr<ExprB> lhs, Token op, std::unique_ptr<ExprB> rhs): lhs(std::move(lhs)), op(op), rhs(std::move(rhs)), form(ASTNS::BinOrExpr::Form::ATA) {}
void ASTNS::BinOrExpr::accept(ASTNS::ExprBVisitor *v) { v->visitBinOrExpr(this); }
bool ASTNS::BinOrExpr::empty() { return false; }
ASTNS::BitAndExpr::BitAndExpr(std::unique_ptr<ExprB> lhs, Token op, std::unique_ptr<ExprB> rhs): lhs(std::move(lhs)), op(op), rhs(std::move(rhs)), form(ASTNS::BitAndExpr::Form::ATA) {}
void ASTNS::BitAndExpr::accept(ASTNS::ExprBVisitor *v) { v->visitBitAndExpr(this); }
bool ASTNS::BitAndExpr::empty() { return false; }
ASTNS::BitOrExpr::BitOrExpr(std::unique_ptr<ExprB> lhs, Token op, std::unique_ptr<ExprB> rhs): lhs(std::move(lhs)), op(op), rhs(std::move(rhs)), form(ASTNS::BitOrExpr::Form::ATA) {}
void ASTNS::BitOrExpr::accept(ASTNS::ExprBVisitor *v) { v->visitBitOrExpr(this); }
bool ASTNS::BitOrExpr::empty() { return false; }
ASTNS::BitShiftExpr::BitShiftExpr(std::unique_ptr<ExprB> lhs, Token op, std::unique_ptr<ExprB> rhs): lhs(std::move(lhs)), op(op), rhs(std::move(rhs)), form(ASTNS::BitShiftExpr::Form::ATA) {}
void ASTNS::BitShiftExpr::accept(ASTNS::ExprBVisitor *v) { v->visitBitShiftExpr(this); }
bool ASTNS::BitShiftExpr::empty() { return false; }
ASTNS::BitXorExpr::BitXorExpr(std::unique_ptr<ExprB> lhs, Token op, std::unique_ptr<ExprB> rhs): lhs(std::move(lhs)), op(op), rhs(std::move(rhs)), form(ASTNS::BitXorExpr::Form::ATA) {}
void ASTNS::BitXorExpr::accept(ASTNS::ExprBVisitor *v) { v->visitBitXorExpr(this); }
bool ASTNS::BitXorExpr::empty() { return false; }
ASTNS::BracedBlock::BracedBlock(Token ocurb, std::unique_ptr<StmtB> stmts, std::unique_ptr<ExprB> implret, Token ccurb): ocurb(ocurb), stmts(std::move(stmts)), implret(std::move(implret)), ccurb(ccurb), form(ASTNS::BracedBlock::Form::TAAT) {}
ASTNS::BracedBlock::BracedBlock(Token ocurb, Token newlopt, std::unique_ptr<StmtB> stmts, std::unique_ptr<ExprB> implret, Token ccurb): ocurb(ocurb), newlopt(newlopt), stmts(std::move(stmts)), implret(std::move(implret)), ccurb(ccurb), form(ASTNS::BracedBlock::Form::TTAAT) {}
ASTNS::BracedBlock::BracedBlock(Token ocurb, Token newlopt, Token indentopt, std::unique_ptr<StmtB> stmts, std::unique_ptr<ExprB> implret, Token dedentopt, Token ccurb): ocurb(ocurb), newlopt(newlopt), indentopt(indentopt), stmts(std::move(stmts)), implret(std::move(implret)), dedentopt(dedentopt), ccurb(ccurb), form(ASTNS::BracedBlock::Form::TTTAATT) {}
void ASTNS::BracedBlock::accept(ASTNS::ExprBVisitor *v) { v->visitBracedBlock(this); }
bool ASTNS::BracedBlock::empty() { return false; }
ASTNS::BuiltinType::BuiltinType(Token type): type(type), form(ASTNS::BuiltinType::Form::T) {}
void ASTNS::BuiltinType::accept(ASTNS::TypeBVisitor *v) { v->visitBuiltinType(this); }
bool ASTNS::BuiltinType::empty() { return false; }
ASTNS::CU::CU(std::unique_ptr<DeclB> dl): dl(std::move(dl)), form(ASTNS::CU::Form::A) {}
ASTNS::CU::CU(): form(ASTNS::CU::Form::EMPTY) {}
void ASTNS::CU::accept(ASTNS::CUBVisitor *v) { v->visitCU(this); }
bool ASTNS::CU::empty() { return form == Form::EMPTY; }
ASTNS::CallExpr::CallExpr(std::unique_ptr<ExprB> callee, Token oparn, std::unique_ptr<ArgB> args, Token cparn): callee(std::move(callee)), oparn(oparn), args(std::move(args)), cparn(cparn), form(ASTNS::CallExpr::Form::ATAT) {}
void ASTNS::CallExpr::accept(ASTNS::ExprBVisitor *v) { v->visitCallExpr(this); }
bool ASTNS::CallExpr::empty() { return false; }
ASTNS::CastExpr::CastExpr(Token oparn, std::unique_ptr<TypeB> type, Token cparn, std::unique_ptr<ExprB> operand): oparn(oparn), type(std::move(type)), cparn(cparn), operand(std::move(operand)), form(ASTNS::CastExpr::Form::TATA) {}
void ASTNS::CastExpr::accept(ASTNS::ExprBVisitor *v) { v->visitCastExpr(this); }
bool ASTNS::CastExpr::empty() { return false; }
ASTNS::CompEQExpr::CompEQExpr(std::unique_ptr<ExprB> lhs, Token op, std::unique_ptr<ExprB> rhs): lhs(std::move(lhs)), op(op), rhs(std::move(rhs)), form(ASTNS::CompEQExpr::Form::ATA) {}
void ASTNS::CompEQExpr::accept(ASTNS::ExprBVisitor *v) { v->visitCompEQExpr(this); }
bool ASTNS::CompEQExpr::empty() { return false; }
ASTNS::CompLGTExpr::CompLGTExpr(std::unique_ptr<ExprB> lhs, Token op, std::unique_ptr<ExprB> rhs): lhs(std::move(lhs)), op(op), rhs(std::move(rhs)), form(ASTNS::CompLGTExpr::Form::ATA) {}
void ASTNS::CompLGTExpr::accept(ASTNS::ExprBVisitor *v) { v->visitCompLGTExpr(this); }
bool ASTNS::CompLGTExpr::empty() { return false; }
ASTNS::DeclList::DeclList(std::unique_ptr<DeclB> decllist, std::unique_ptr<DeclB> anotherdecl): decllist(std::move(decllist)), anotherdecl(std::move(anotherdecl)), form(ASTNS::DeclList::Form::AA) {}
void ASTNS::DeclList::accept(ASTNS::DeclBVisitor *v) { v->visitDeclList(this); }
bool ASTNS::DeclList::empty() { return false; }
ASTNS::ExprStmt::ExprStmt(std::unique_ptr<ExprB> expr): expr(std::move(expr)), form(ASTNS::ExprStmt::Form::A) {}
void ASTNS::ExprStmt::accept(ASTNS::StmtBVisitor *v) { v->visitExprStmt(this); }
bool ASTNS::ExprStmt::empty() { return false; }
ASTNS::FunctionDecl::FunctionDecl(Token fun, std::unique_ptr<TypeB> retty, Token name, Token oparn, std::unique_ptr<PListB> paramlist, Token cparn, std::unique_ptr<ExprB> body): fun(fun), retty(std::move(retty)), name(name), oparn(oparn), paramlist(std::move(paramlist)), cparn(cparn), body(std::move(body)), form(ASTNS::FunctionDecl::Form::TATTATA) {}
ASTNS::FunctionDecl::FunctionDecl(Token fun, std::unique_ptr<TypeB> retty, Token name, Token oparn, std::unique_ptr<PListB> paramlist, Token cparn, Token newl): fun(fun), retty(std::move(retty)), name(name), oparn(oparn), paramlist(std::move(paramlist)), cparn(cparn), newl(newl), form(ASTNS::FunctionDecl::Form::TATTATT) {}
void ASTNS::FunctionDecl::accept(ASTNS::DeclBVisitor *v) { v->visitFunctionDecl(this); }
bool ASTNS::FunctionDecl::empty() { return false; }
ASTNS::ImplRet::ImplRet(Token leftarrow, std::unique_ptr<ExprB> expr, std::unique_ptr<StmtEndingB> ending): leftarrow(leftarrow), expr(std::move(expr)), ending(std::move(ending)), form(ASTNS::ImplRet::Form::TAA) {}
void ASTNS::ImplRet::accept(ASTNS::ExprBVisitor *v) { v->visitImplRet(this); }
bool ASTNS::ImplRet::empty() { return false; }
ASTNS::ImplRet_OPT::ImplRet_OPT(): form(ASTNS::ImplRet_OPT::Form::EMPTY) {}
void ASTNS::ImplRet_OPT::accept(ASTNS::ExprBVisitor *v) { v->visitImplRet_OPT(this); }
bool ASTNS::ImplRet_OPT::empty() { return form == Form::EMPTY; }
ASTNS::IndentedBlock::IndentedBlock(Token newl, Token indent, std::unique_ptr<StmtB> stmts, std::unique_ptr<ExprB> implret, Token dedent): newl(newl), indent(indent), stmts(std::move(stmts)), implret(std::move(implret)), dedent(dedent), form(ASTNS::IndentedBlock::Form::TTAAT) {}
void ASTNS::IndentedBlock::accept(ASTNS::ExprBVisitor *v) { v->visitIndentedBlock(this); }
bool ASTNS::IndentedBlock::empty() { return false; }
ASTNS::MultExpr::MultExpr(std::unique_ptr<ExprB> lhs, Token op, std::unique_ptr<ExprB> rhs): lhs(std::move(lhs)), op(op), rhs(std::move(rhs)), form(ASTNS::MultExpr::Form::ATA) {}
void ASTNS::MultExpr::accept(ASTNS::ExprBVisitor *v) { v->visitMultExpr(this); }
bool ASTNS::MultExpr::empty() { return false; }
ASTNS::Param::Param(std::unique_ptr<TypeB> type, Token name): type(std::move(type)), name(name), form(ASTNS::Param::Form::AT) {}
void ASTNS::Param::accept(ASTNS::PListBVisitor *v) { v->visitParam(this); }
bool ASTNS::Param::empty() { return false; }
ASTNS::ParamList::ParamList(std::unique_ptr<PListB> paramsegment, Token comma): paramsegment(std::move(paramsegment)), comma(comma), form(ASTNS::ParamList::Form::AT) {}
void ASTNS::ParamList::accept(ASTNS::PListBVisitor *v) { v->visitParamList(this); }
bool ASTNS::ParamList::empty() { return false; }
ASTNS::ParamList_OPT::ParamList_OPT(): form(ASTNS::ParamList_OPT::Form::EMPTY) {}
void ASTNS::ParamList_OPT::accept(ASTNS::PListBVisitor *v) { v->visitParamList_OPT(this); }
bool ASTNS::ParamList_OPT::empty() { return form == Form::EMPTY; }
ASTNS::ParamSegment::ParamSegment(std::unique_ptr<PListB> paramsegment, Token comma, std::unique_ptr<PListB> anotherparam): paramsegment(std::move(paramsegment)), comma(comma), anotherparam(std::move(anotherparam)), form(ASTNS::ParamSegment::Form::ATA) {}
void ASTNS::ParamSegment::accept(ASTNS::PListBVisitor *v) { v->visitParamSegment(this); }
bool ASTNS::ParamSegment::empty() { return false; }
ASTNS::PrimaryExpr::PrimaryExpr(Token value): value(value), form(ASTNS::PrimaryExpr::Form::T) {}
ASTNS::PrimaryExpr::PrimaryExpr(Token oparn, std::unique_ptr<ExprB> expr, Token cparn): oparn(oparn), expr(std::move(expr)), cparn(cparn), form(ASTNS::PrimaryExpr::Form::TAT) {}
void ASTNS::PrimaryExpr::accept(ASTNS::ExprBVisitor *v) { v->visitPrimaryExpr(this); }
bool ASTNS::PrimaryExpr::empty() { return false; }
ASTNS::RetStmt::RetStmt(Token ret, std::unique_ptr<ExprB> expr): ret(ret), expr(std::move(expr)), form(ASTNS::RetStmt::Form::TA) {}
ASTNS::RetStmt::RetStmt(Token ret): ret(ret), form(ASTNS::RetStmt::Form::T) {}
void ASTNS::RetStmt::accept(ASTNS::StmtBVisitor *v) { v->visitRetStmt(this); }
bool ASTNS::RetStmt::empty() { return false; }
ASTNS::StmtEnding::StmtEnding(Token tok): tok(tok), form(ASTNS::StmtEnding::Form::T) {}
ASTNS::StmtEnding::StmtEnding(Token tok, Token tok2): tok(tok), tok2(tok2), form(ASTNS::StmtEnding::Form::TT) {}
void ASTNS::StmtEnding::accept(ASTNS::StmtEndingBVisitor *v) { v->visitStmtEnding(this); }
bool ASTNS::StmtEnding::empty() { return false; }
ASTNS::StmtEnding_OPT::StmtEnding_OPT(): form(ASTNS::StmtEnding_OPT::Form::EMPTY) {}
void ASTNS::StmtEnding_OPT::accept(ASTNS::StmtEndingBVisitor *v) { v->visitStmtEnding_OPT(this); }
bool ASTNS::StmtEnding_OPT::empty() { return form == Form::EMPTY; }
ASTNS::StmtList::StmtList(std::unique_ptr<StmtB> stmtsegment, std::unique_ptr<StmtEndingB> stmtending): stmtsegment(std::move(stmtsegment)), stmtending(std::move(stmtending)), form(ASTNS::StmtList::Form::AA) {}
void ASTNS::StmtList::accept(ASTNS::StmtBVisitor *v) { v->visitStmtList(this); }
bool ASTNS::StmtList::empty() { return false; }
ASTNS::StmtList_OPT::StmtList_OPT(): form(ASTNS::StmtList_OPT::Form::EMPTY) {}
void ASTNS::StmtList_OPT::accept(ASTNS::StmtBVisitor *v) { v->visitStmtList_OPT(this); }
bool ASTNS::StmtList_OPT::empty() { return form == Form::EMPTY; }
ASTNS::StmtSegment::StmtSegment(std::unique_ptr<StmtB> stmtsegment, std::unique_ptr<StmtEndingB> stmtending, std::unique_ptr<StmtB> anotherstmt): stmtsegment(std::move(stmtsegment)), stmtending(std::move(stmtending)), anotherstmt(std::move(anotherstmt)), form(ASTNS::StmtSegment::Form::AAA) {}
void ASTNS::StmtSegment::accept(ASTNS::StmtBVisitor *v) { v->visitStmtSegment(this); }
bool ASTNS::StmtSegment::empty() { return false; }
ASTNS::TernaryExpr::TernaryExpr(std::unique_ptr<ExprB> cond, Token quest, std::unique_ptr<ExprB> trues, Token colon, std::unique_ptr<ExprB> falses): cond(std::move(cond)), quest(quest), trues(std::move(trues)), colon(colon), falses(std::move(falses)), form(ASTNS::TernaryExpr::Form::ATATA) {}
void ASTNS::TernaryExpr::accept(ASTNS::ExprBVisitor *v) { v->visitTernaryExpr(this); }
bool ASTNS::TernaryExpr::empty() { return false; }
ASTNS::UnaryExpr::UnaryExpr(Token op, std::unique_ptr<ExprB> operand): op(op), operand(std::move(operand)), form(ASTNS::UnaryExpr::Form::TA) {}
void ASTNS::UnaryExpr::accept(ASTNS::ExprBVisitor *v) { v->visitUnaryExpr(this); }
bool ASTNS::UnaryExpr::empty() { return false; }
ASTNS::VarStmt::VarStmt(Token var, std::unique_ptr<TypeB> type, std::unique_ptr<VStmtIB> assignments): var(var), type(std::move(type)), assignments(std::move(assignments)), form(ASTNS::VarStmt::Form::TAA) {}
void ASTNS::VarStmt::accept(ASTNS::StmtBVisitor *v) { v->visitVarStmt(this); }
bool ASTNS::VarStmt::empty() { return false; }
ASTNS::VarStmtItem::VarStmtItem(Token name, Token equal, std::unique_ptr<ExprB> expr): name(name), equal(equal), expr(std::move(expr)), form(ASTNS::VarStmtItem::Form::TTA) {}
ASTNS::VarStmtItem::VarStmtItem(Token name): name(name), form(ASTNS::VarStmtItem::Form::T) {}
void ASTNS::VarStmtItem::accept(ASTNS::VStmtIBVisitor *v) { v->visitVarStmtItem(this); }
bool ASTNS::VarStmtItem::empty() { return false; }
ASTNS::VarStmtItemList::VarStmtItemList(std::unique_ptr<VStmtIB> varstmtitemsegment, Token comma): varstmtitemsegment(std::move(varstmtitemsegment)), comma(comma), form(ASTNS::VarStmtItemList::Form::AT) {}
void ASTNS::VarStmtItemList::accept(ASTNS::VStmtIBVisitor *v) { v->visitVarStmtItemList(this); }
bool ASTNS::VarStmtItemList::empty() { return false; }
ASTNS::VarStmtItemSegment::VarStmtItemSegment(std::unique_ptr<VStmtIB> varstmtitemsegment, Token comma, std::unique_ptr<VStmtIB> anothervarstmtitem): varstmtitemsegment(std::move(varstmtitemsegment)), comma(comma), anothervarstmtitem(std::move(anothervarstmtitem)), form(ASTNS::VarStmtItemSegment::Form::ATA) {}
void ASTNS::VarStmtItemSegment::accept(ASTNS::VStmtIBVisitor *v) { v->visitVarStmtItemSegment(this); }
bool ASTNS::VarStmtItemSegment::empty() { return false; }
// This code was autogenerated - see the utils/ directory

// ASTCPP END
