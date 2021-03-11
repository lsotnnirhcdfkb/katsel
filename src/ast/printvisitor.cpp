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

static void print_field(ASTNS::PrintVisitor &p, Token const &tok) {
    p.pai(format("token: {}\n", tok.stringify_type()));
}

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
    pai("Located<Token> name = ");
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
    pai("Located<Token> name = ");
    print_field(*this, a.name);
    pai("Maybe<Located<Token>> equal = ");
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
    pai("Located<Token> th = ");
    print_field(*this, a.th);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::ast_visit(ASTNS::Param &a) {
    pai("Param {\n");
    ++indent;
    pai("std::unique_ptr<Type> type = ");
    print_field(*this, a.type);
    pai("Located<Token> name = ");
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
    pai("Located<Token> iftok = ");
    print_field(*this, a.iftok);
    pai("Maybe<Located<Token>> elsetok = ");
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
    pai("Located<Token> op = ");
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
    pai("Located<Token> op = ");
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
    pai("Located<Token> oparn = ");
    print_field(*this, a.oparn);
    pai("std::vector<std::unique_ptr<Expr>> args = ");
    print_field(*this, a.args);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::ast_visit(ASTNS::FieldAccessExpr &a) {
    pai("FieldAccessExpr {\n");
    ++indent;
    pai("std::unique_ptr<Expr> operand = ");
    print_field(*this, a.operand);
    pai("Located<Token> dot = ");
    print_field(*this, a.dot);
    pai("Located<Token> field = ");
    print_field(*this, a.field);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::ast_visit(ASTNS::MethodCallExpr &a) {
    pai("MethodCallExpr {\n");
    ++indent;
    pai("std::unique_ptr<Expr> operand = ");
    print_field(*this, a.operand);
    pai("Located<Token> dot = ");
    print_field(*this, a.dot);
    pai("Located<Token> method = ");
    print_field(*this, a.method);
    pai("Located<Token> oparn = ");
    print_field(*this, a.oparn);
    pai("std::vector<std::unique_ptr<Expr>> args = ");
    print_field(*this, a.args);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::ast_visit(ASTNS::BoolLit &a) {
    pai("BoolLit {\n");
    ++indent;
    pai("Located<Token> val = ");
    print_field(*this, a.val);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::ast_visit(ASTNS::FloatLit &a) {
    pai("FloatLit {\n");
    ++indent;
    pai("Located<Token> val = ");
    print_field(*this, a.val);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::ast_visit(ASTNS::IntLit &a) {
    pai("IntLit {\n");
    ++indent;
    pai("Located<Token> val = ");
    print_field(*this, a.val);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::ast_visit(ASTNS::CharLit &a) {
    pai("CharLit {\n");
    ++indent;
    pai("Located<Token> val = ");
    print_field(*this, a.val);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::ast_visit(ASTNS::StringLit &a) {
    pai("StringLit {\n");
    ++indent;
    pai("Located<Token> val = ");
    print_field(*this, a.val);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::ast_visit(ASTNS::ThisExpr &a) {
    pai("ThisExpr {\n");
    ++indent;
    pai("Located<Token> tok = ");
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
    pai("std::vector<Located<Token>> segments = ");
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
