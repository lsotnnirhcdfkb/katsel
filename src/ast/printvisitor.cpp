#include "ast/printvisitor.h"
#include "ast/ast.h"

#include "utils/format.h"

#include <iostream>
#include <string>

template <typename T, typename = std::enable_if_t<std::is_base_of_v<ASTNS::AST, T>>>
static void print_field(ASTNS::PrintVisitor &p, std::unique_ptr<T> const &ast) {
    if (ast)
        ast->ast_accept(p);
    else
        p.pai("nullptr\n");
}

static void print_field(ASTNS::PrintVisitor &p, int i) {
    p.pai(std::to_string(i));
    p.pai("\n");
}
static void print_field(ASTNS::PrintVisitor &p, bool i) {
    p.pai(i ? "true" : "false");
    p.pai("\n");
}
static void print_field(ASTNS::PrintVisitor &p, ASTNS::BinaryOperator b) {
    p.pai(format("binary operator = {}\n", static_cast<int>(b)));
}
static void print_field(ASTNS::PrintVisitor &p, ASTNS::ShortCircuitOperator s) {
    p.pai(format("short circuit operator = {}\n", static_cast<int>(s)));
}
static void print_field(ASTNS::PrintVisitor &p, ASTNS::UnaryOperator u) {
    p.pai(format("unary operator = {}\n", static_cast<int>(u)));
}
static void print_field(ASTNS::PrintVisitor &p, ASTNS::AssignOperator a) {
    p.pai(format("assign operator = {}\n", static_cast<int>(a)));
}

static void print_field(ASTNS::PrintVisitor &p, Span const &s) {
    p.pai(format("span at {}: {}\n", s.as_rowcol(), s.stringify()));
}

// tokens {{{
static void print_field(ASTNS::PrintVisitor &p, Tokens::OParen const &tok) { p.pai(Tokens::OParen::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::CParen const &tok) { p.pai(Tokens::CParen::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::OBrack const &tok) { p.pai(Tokens::OBrack::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::CBrack const &tok) { p.pai(Tokens::CBrack::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::OBrace const &tok) { p.pai(Tokens::OBrace::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::CBrace const &tok) { p.pai(Tokens::CBrace::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::Comma const &tok) { p.pai(Tokens::Comma::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::Period const &tok) { p.pai(Tokens::Period::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::Semicolon const &tok) { p.pai(Tokens::Semicolon::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::Question const &tok) { p.pai(Tokens::Question::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::Colon const &tok) { p.pai(Tokens::Colon::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::Bang const &tok) { p.pai(Tokens::Bang::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::Plus const &tok) { p.pai(Tokens::Plus::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::Minus const &tok) { p.pai(Tokens::Minus::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::Star const &tok) { p.pai(Tokens::Star::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::Slash const &tok) { p.pai(Tokens::Slash::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::Percent const &tok) { p.pai(Tokens::Percent::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::Equal const &tok) { p.pai(Tokens::Equal::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::Greater const &tok) { p.pai(Tokens::Greater::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::Less const &tok) { p.pai(Tokens::Less::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::Tilde const &tok) { p.pai(Tokens::Tilde::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::Amper const &tok) { p.pai(Tokens::Amper::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::Pipe const &tok) { p.pai(Tokens::Pipe::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::Caret const &tok) { p.pai(Tokens::Caret::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::Dollar const &tok) { p.pai(Tokens::Dollar::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::Hash const &tok) { p.pai(Tokens::Hash::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::RightArrow const &tok) { p.pai(Tokens::RightArrow::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::LeftArrow const &tok) { p.pai(Tokens::LeftArrow::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::DoublePlus const &tok) { p.pai(Tokens::DoublePlus::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::DoubleMinus const &tok) { p.pai(Tokens::DoubleMinus::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::DoubleGreater const &tok) { p.pai(Tokens::DoubleGreater::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::DoubleLess const &tok) { p.pai(Tokens::DoubleLess::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::DoubleAmper const &tok) { p.pai(Tokens::DoubleAmper::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::DoublePipe const &tok) { p.pai(Tokens::DoublePipe::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::DoubleEqual const &tok) { p.pai(Tokens::DoubleEqual::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::DoubleColon const &tok) { p.pai(Tokens::DoubleColon::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::PlusEqual const &tok) { p.pai(Tokens::PlusEqual::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::MinusEqual const &tok) { p.pai(Tokens::MinusEqual::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::StarEqual const &tok) { p.pai(Tokens::StarEqual::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::SlashEqual const &tok) { p.pai(Tokens::SlashEqual::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::BangEqual const &tok) { p.pai(Tokens::BangEqual::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::GreaterEqual const &tok) { p.pai(Tokens::GreaterEqual::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::LessEqual const &tok) { p.pai(Tokens::LessEqual::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::PercentEqual const &tok) { p.pai(Tokens::PercentEqual::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::DoubleLessEqual const &tok) { p.pai(Tokens::DoubleLessEqual::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::DoubleGreaterEqual const &tok) { p.pai(Tokens::DoubleGreaterEqual::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::AmperEqual const &tok) { p.pai(Tokens::AmperEqual::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::PipeEqual const &tok) { p.pai(Tokens::PipeEqual::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::CaretEqual const &tok) { p.pai(Tokens::CaretEqual::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::Identifier const &tok) { p.pai(Tokens::Identifier::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::CharLit const &tok) { p.pai(Tokens::CharLit::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::StringLit const &tok) { p.pai(Tokens::StringLit::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::IntLit const &tok) { p.pai(Tokens::IntLit::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::FloatLit const &tok) { p.pai(Tokens::FloatLit::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::BoolLit const &tok) { p.pai(Tokens::BoolLit::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::This const &tok) { p.pai(Tokens::This::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::Var const &tok) { p.pai(Tokens::Var::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::Fun const &tok) { p.pai(Tokens::Fun::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::Let const &tok) { p.pai(Tokens::Let::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::Mut const &tok) { p.pai(Tokens::Mut::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::Data const &tok) { p.pai(Tokens::Data::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::Impl const &tok) { p.pai(Tokens::Impl::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::Return const &tok) { p.pai(Tokens::Return::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::While const &tok) { p.pai(Tokens::While::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::For const &tok) { p.pai(Tokens::For::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::If const &tok) { p.pai(Tokens::If::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::Else const &tok) { p.pai(Tokens::Else::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::Case const &tok) { p.pai(Tokens::Case::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::Break const &tok) { p.pai(Tokens::Break::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::Continue const &tok) { p.pai(Tokens::Continue::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::Boom const &tok) { p.pai(Tokens::Boom::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::Newline const &tok) { p.pai(Tokens::Newline::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::Indent const &tok) { p.pai(Tokens::Indent::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::Dedent const &tok) { p.pai(Tokens::Dedent::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::_EOF const &tok) { p.pai(Tokens::_EOF::stringify()); p.pai("\n"); }
static void print_field(ASTNS::PrintVisitor &p, Tokens::Error const &tok) { p.pai(Tokens::Error::stringify()); p.pai("\n"); }
// }}}

template <typename T>
static void print_field(ASTNS::PrintVisitor &p, Located<T> const &l) {
    p.pai("<located: ");
    print_field(p, l.value);
    p.pai(" at ");
    print_field(p, l.span);
}

template <typename T>
static void print_field(ASTNS::PrintVisitor &p, Maybe<T> const &m) {
    if (m.has()) {
        p.pai("<maybe with { ");
        print_field(p, m.get());
        p.pai(" }>\n");
    }
    else {
        p.pai("<maybe not>\n");
    }
}

template <typename T>
static void print_field(ASTNS::PrintVisitor &p, std::vector<T> const &v) {
    p.pai("[\n");
    ++p.indent;
    for (T const &t : v)
        print_field(p, t);
    --p.indent;
    p.pai("]\n");
}

// PRINTVISITOR START
void ASTNS::PrintVisitor::ast_visit(ASTNS::DeclList &a) {
    pai("DeclList {\n");
    ++indent;
    pai("std::vector<std::unique_ptr<Decl>> decls = ");
    print_field(*this, a.decls);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::ast_visit(ASTNS::StmtList &a) {
    pai("StmtList {\n");
    ++indent;
    pai("std::vector<std::unique_ptr<Stmt>> stmts = ");
    print_field(*this, a.stmts);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::ast_visit(ASTNS::ParamList &a) {
    pai("ParamList {\n");
    ++indent;
    pai("std::vector<std::unique_ptr<ParamB>> params = ");
    print_field(*this, a.params);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::ast_visit(ASTNS::ArgList &a) {
    pai("ArgList {\n");
    ++indent;
    pai("std::vector<std::unique_ptr<Arg>> args = ");
    print_field(*this, a.args);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::ast_visit(ASTNS::ImplMemberList &a) {
    pai("ImplMemberList {\n");
    ++indent;
    pai("std::vector<std::unique_ptr<ImplMember>> members = ");
    print_field(*this, a.members);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::ast_visit(ASTNS::PureLocation &a) {
    pai("PureLocation {\n");
    ++indent;
    pai("int dummy = ");
    print_field(*this, a.dummy);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::ast_visit(ASTNS::ImplicitDecl &a) {
    pai("ImplicitDecl {\n");
    ++indent;
    pai("int dummy = ");
    print_field(*this, a.dummy);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::ast_visit(ASTNS::CU &a) {
    pai("CU {\n");
    ++indent;
    pai("std::vector<std::unique_ptr<Decl>> decls = ");
    print_field(*this, a.decls);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::ast_visit(ASTNS::ImplDecl &a) {
    pai("ImplDecl {\n");
    ++indent;
    pai("std::unique_ptr<Type> impl_for = ");
    print_field(*this, a.impl_for);
    pai("std::vector<std::unique_ptr<ImplMember>> members = ");
    print_field(*this, a.members);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::ast_visit(ASTNS::FunctionDecl &a) {
    pai("FunctionDecl {\n");
    ++indent;
    pai("std::unique_ptr<Type> retty = ");
    print_field(*this, a.retty);
    pai("Located<Tokens::Identifier> name = ");
    print_field(*this, a.name);
    pai("std::vector<std::unique_ptr<ParamB>> params = ");
    print_field(*this, a.params);
    pai("std::unique_ptr<Block> body = ");
    print_field(*this, a.body);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::ast_visit(ASTNS::FunctionImplMember &a) {
    pai("FunctionImplMember {\n");
    ++indent;
    pai("std::unique_ptr<FunctionDecl> fun = ");
    print_field(*this, a.fun);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::ast_visit(ASTNS::VarStmt &a) {
    pai("VarStmt {\n");
    ++indent;
    pai("std::unique_ptr<Type> type = ");
    print_field(*this, a.type);
    pai("bool mut = ");
    print_field(*this, a.mut);
    pai("Located<Tokens::Identifier> name = ");
    print_field(*this, a.name);
    pai("Maybe<Located<Tokens::Equal>> equal = ");
    print_field(*this, a.equal);
    pai("std::unique_ptr<Expr> expr = ");
    print_field(*this, a.expr);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::ast_visit(ASTNS::ExprStmt &a) {
    pai("ExprStmt {\n");
    ++indent;
    pai("std::unique_ptr<Expr> expr = ");
    print_field(*this, a.expr);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::ast_visit(ASTNS::RetStmt &a) {
    pai("RetStmt {\n");
    ++indent;
    pai("std::unique_ptr<Expr> expr = ");
    print_field(*this, a.expr);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::ast_visit(ASTNS::PathType &a) {
    pai("PathType {\n");
    ++indent;
    pai("std::unique_ptr<Path> path = ");
    print_field(*this, a.path);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::ast_visit(ASTNS::PointerType &a) {
    pai("PointerType {\n");
    ++indent;
    pai("bool mut = ");
    print_field(*this, a.mut);
    pai("std::unique_ptr<Type> type = ");
    print_field(*this, a.type);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::ast_visit(ASTNS::ThisType &a) {
    pai("ThisType {\n");
    ++indent;
    pai("Located<Tokens::This> th = ");
    print_field(*this, a.th);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::ast_visit(ASTNS::Arg &a) {
    pai("Arg {\n");
    ++indent;
    pai("std::unique_ptr<Expr> expr = ");
    print_field(*this, a.expr);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::ast_visit(ASTNS::Param &a) {
    pai("Param {\n");
    ++indent;
    pai("std::unique_ptr<Type> type = ");
    print_field(*this, a.type);
    pai("Located<Tokens::Identifier> name = ");
    print_field(*this, a.name);
    pai("bool mut = ");
    print_field(*this, a.mut);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::ast_visit(ASTNS::ThisParam &a) {
    pai("ThisParam {\n");
    ++indent;
    pai("bool ptr = ");
    print_field(*this, a.ptr);
    pai("bool mut = ");
    print_field(*this, a.mut);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::ast_visit(ASTNS::Block &a) {
    pai("Block {\n");
    ++indent;
    pai("std::vector<std::unique_ptr<Stmt>> stmts = ");
    print_field(*this, a.stmts);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::ast_visit(ASTNS::IfExpr &a) {
    pai("IfExpr {\n");
    ++indent;
    pai("Located<Tokens::If> iftok = ");
    print_field(*this, a.iftok);
    pai("Maybe<Located<Tokens::Else>> elsetok = ");
    print_field(*this, a.elsetok);
    pai("std::unique_ptr<Expr> cond = ");
    print_field(*this, a.cond);
    pai("std::unique_ptr<Expr> trues = ");
    print_field(*this, a.trues);
    pai("std::unique_ptr<Expr> falses = ");
    print_field(*this, a.falses);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::ast_visit(ASTNS::WhileExpr &a) {
    pai("WhileExpr {\n");
    ++indent;
    pai("std::unique_ptr<Expr> cond = ");
    print_field(*this, a.cond);
    pai("std::unique_ptr<Expr> body = ");
    print_field(*this, a.body);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::ast_visit(ASTNS::AssignmentExpr &a) {
    pai("AssignmentExpr {\n");
    ++indent;
    pai("std::unique_ptr<Expr> target = ");
    print_field(*this, a.target);
    pai("Located<AssignOperator> equal = ");
    print_field(*this, a.equal);
    pai("std::unique_ptr<Expr> expr = ");
    print_field(*this, a.expr);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::ast_visit(ASTNS::ShortCircuitExpr &a) {
    pai("ShortCircuitExpr {\n");
    ++indent;
    pai("std::unique_ptr<Expr> lhs = ");
    print_field(*this, a.lhs);
    pai("Located<ShortCircuitOperator> op = ");
    print_field(*this, a.op);
    pai("std::unique_ptr<Expr> rhs = ");
    print_field(*this, a.rhs);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::ast_visit(ASTNS::BinaryExpr &a) {
    pai("BinaryExpr {\n");
    ++indent;
    pai("std::unique_ptr<Expr> lhs = ");
    print_field(*this, a.lhs);
    pai("Located<BinaryOperator> op = ");
    print_field(*this, a.op);
    pai("std::unique_ptr<Expr> rhs = ");
    print_field(*this, a.rhs);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::ast_visit(ASTNS::CastExpr &a) {
    pai("CastExpr {\n");
    ++indent;
    pai("std::unique_ptr<Type> type = ");
    print_field(*this, a.type);
    pai("std::unique_ptr<Expr> expr = ");
    print_field(*this, a.expr);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::ast_visit(ASTNS::UnaryExpr &a) {
    pai("UnaryExpr {\n");
    ++indent;
    pai("Located<UnaryOperator> op = ");
    print_field(*this, a.op);
    pai("std::unique_ptr<Expr> expr = ");
    print_field(*this, a.expr);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::ast_visit(ASTNS::AddrofExpr &a) {
    pai("AddrofExpr {\n");
    ++indent;
    pai("Located<Tokens::Amper> op = ");
    print_field(*this, a.op);
    pai("std::unique_ptr<Expr> expr = ");
    print_field(*this, a.expr);
    pai("bool mut = ");
    print_field(*this, a.mut);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::ast_visit(ASTNS::DerefExpr &a) {
    pai("DerefExpr {\n");
    ++indent;
    pai("Located<Tokens::Star> op = ");
    print_field(*this, a.op);
    pai("std::unique_ptr<Expr> expr = ");
    print_field(*this, a.expr);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::ast_visit(ASTNS::CallExpr &a) {
    pai("CallExpr {\n");
    ++indent;
    pai("std::unique_ptr<Expr> callee = ");
    print_field(*this, a.callee);
    pai("Located<Tokens::OParen> oparn = ");
    print_field(*this, a.oparn);
    pai("std::vector<std::unique_ptr<Arg>> args = ");
    print_field(*this, a.args);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::ast_visit(ASTNS::FieldAccessExpr &a) {
    pai("FieldAccessExpr {\n");
    ++indent;
    pai("std::unique_ptr<Expr> operand = ");
    print_field(*this, a.operand);
    pai("Located<Tokens::Period> dot = ");
    print_field(*this, a.dot);
    pai("Located<Tokens::Identifier> field = ");
    print_field(*this, a.field);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::ast_visit(ASTNS::MethodCallExpr &a) {
    pai("MethodCallExpr {\n");
    ++indent;
    pai("std::unique_ptr<Expr> operand = ");
    print_field(*this, a.operand);
    pai("Located<Tokens::Period> dot = ");
    print_field(*this, a.dot);
    pai("Located<Tokens::Identifier> method = ");
    print_field(*this, a.method);
    pai("Located<Tokens::OParen> oparn = ");
    print_field(*this, a.oparn);
    pai("std::vector<std::unique_ptr<Arg>> args = ");
    print_field(*this, a.args);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::ast_visit(ASTNS::BoolLit &a) {
    pai("BoolLit {\n");
    ++indent;
    pai("Located<Tokens::BoolLit> val = ");
    print_field(*this, a.val);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::ast_visit(ASTNS::FloatLit &a) {
    pai("FloatLit {\n");
    ++indent;
    pai("Located<Tokens::FloatLit> val = ");
    print_field(*this, a.val);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::ast_visit(ASTNS::IntLit &a) {
    pai("IntLit {\n");
    ++indent;
    pai("Located<Tokens::IntLit> val = ");
    print_field(*this, a.val);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::ast_visit(ASTNS::CharLit &a) {
    pai("CharLit {\n");
    ++indent;
    pai("Located<Tokens::CharLit> val = ");
    print_field(*this, a.val);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::ast_visit(ASTNS::StringLit &a) {
    pai("StringLit {\n");
    ++indent;
    pai("Located<Tokens::StringLit> val = ");
    print_field(*this, a.val);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::ast_visit(ASTNS::ThisExpr &a) {
    pai("ThisExpr {\n");
    ++indent;
    pai("Located<Tokens::This> tok = ");
    print_field(*this, a.tok);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::ast_visit(ASTNS::PathExpr &a) {
    pai("PathExpr {\n");
    ++indent;
    pai("std::unique_ptr<Path> path = ");
    print_field(*this, a.path);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::ast_visit(ASTNS::Path &a) {
    pai("Path {\n");
    ++indent;
    pai("std::vector<Located<Tokens::Identifier>> segments = ");
    print_field(*this, a.segments);
    --indent;
    pai("}\n");
}
// PRINTVISITOR END

void ASTNS::PrintVisitor::pai(std::string const &s) {
    for (auto i = s.begin(); i != s.end(); ++i) {
        if (pindent)
            for (int j = 0; j < indent; ++j)
                ostream << "    ";

        pindent = false;
        ostream << *i;

        if (*i == '\n')
            pindent = true;
    }
}

ASTNS::PrintVisitor::PrintVisitor(llvm::raw_ostream &ostream): ostream(ostream), indent(0) {}
