#include "ast/printvisitor.h"
#include "ast/ast.h"

#include "utils/format.h"

#include <iostream>
#include <string>

template <typename T>
static void print_field(ASTNS::PrintVisitor &p, std::unique_ptr<T> &ast) {
    if (ast)
        ast->accept(p);
    else
        p.pai("nullptr\n");
}
template <typename T>
static void print_field(ASTNS::PrintVisitor &p, std::vector<std::unique_ptr<T>> &v) {
    p.pai("[\n");
    ++p.indent;
    for (std::unique_ptr<T> &a : v)
        a->accept(p);
    --p.indent;
    p.pai("]\n");
}
static void print_field(ASTNS::PrintVisitor &p, Token const &t) {
    p.pai("\"");
    p.pai(t.span.stringify());
    p.pai("\"\n");
}
static void print_field(ASTNS::PrintVisitor &p, int i) {
    p.pai(std::to_string(i));
    p.pai("\n");
}
static void print_field(ASTNS::PrintVisitor &p, bool i) {
    p.pai(i ? "true" : "false");
    p.pai("\n");
}
static void print_field(ASTNS::PrintVisitor &p, std::vector<Token> &v) {
    p.pai("[");
    bool first = true;
    for (Token const &t : v) {
        if (!first)
            p.pai(", ");
        p.pai("\"");
        p.pai(t.span.stringify());
        p.pai("\"");

        first = false;
    }
    p.pai("]\n");
}
static void print_field(ASTNS::PrintVisitor &p, Maybe<Span const> const &s) {
    p.pai(format("<maybe span: {}>\n", s.has()));
}

// PRINTVISITOR START
void ASTNS::PrintVisitor::visit(ASTNS::DeclList &a) {
    pai("DeclList {\n");
    ++indent;
    pai("std::vector<std::unique_ptr<Decl>> decls = ");
    print_field(*this, a.decls);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visit(ASTNS::StmtList &a) {
    pai("StmtList {\n");
    ++indent;
    pai("std::vector<std::unique_ptr<Stmt>> stmts = ");
    print_field(*this, a.stmts);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visit(ASTNS::ParamList &a) {
    pai("ParamList {\n");
    ++indent;
    pai("std::vector<std::unique_ptr<ParamB>> params = ");
    print_field(*this, a.params);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visit(ASTNS::ArgList &a) {
    pai("ArgList {\n");
    ++indent;
    pai("std::vector<std::unique_ptr<Arg>> args = ");
    print_field(*this, a.args);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visit(ASTNS::VarStmtItemList &a) {
    pai("VarStmtItemList {\n");
    ++indent;
    pai("std::vector<std::unique_ptr<VarStmtItem>> items = ");
    print_field(*this, a.items);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visit(ASTNS::ImplMemberList &a) {
    pai("ImplMemberList {\n");
    ++indent;
    pai("std::vector<std::unique_ptr<ImplMember>> members = ");
    print_field(*this, a.members);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visit(ASTNS::PureLocation &a) {
    pai("PureLocation {\n");
    ++indent;
    pai("int dummy = ");
    print_field(*this, a.dummy);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visit(ASTNS::ImplicitDecl &a) {
    pai("ImplicitDecl {\n");
    ++indent;
    pai("int dummy = ");
    print_field(*this, a.dummy);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visit(ASTNS::CU &a) {
    pai("CU {\n");
    ++indent;
    pai("std::vector<std::unique_ptr<Decl>> decls = ");
    print_field(*this, a.decls);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visit(ASTNS::ImplDecl &a) {
    pai("ImplDecl {\n");
    ++indent;
    pai("std::unique_ptr<Type> impl_for = ");
    print_field(*this, a.impl_for);
    pai("std::vector<std::unique_ptr<ImplMember>> members = ");
    print_field(*this, a.members);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visit(ASTNS::FunctionDecl &a) {
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
void ASTNS::PrintVisitor::visit(ASTNS::FunctionImplMember &a) {
    pai("FunctionImplMember {\n");
    ++indent;
    pai("std::unique_ptr<FunctionDecl> fun = ");
    print_field(*this, a.fun);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visit(ASTNS::VarStmt &a) {
    pai("VarStmt {\n");
    ++indent;
    pai("std::vector<std::unique_ptr<VarStmtItem>> items = ");
    print_field(*this, a.items);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visit(ASTNS::VarStmtItem &a) {
    pai("VarStmtItem {\n");
    ++indent;
    pai("std::unique_ptr<Type> type = ");
    print_field(*this, a.type);
    pai("bool mut = ");
    print_field(*this, a.mut);
    pai("Located<Tokens::Identifier> name = ");
    print_field(*this, a.name);
    pai("Located<Tokens::Equal> equal = ");
    print_field(*this, a.equal);
    pai("std::unique_ptr<Expr> expr = ");
    print_field(*this, a.expr);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visit(ASTNS::ExprStmt &a) {
    pai("ExprStmt {\n");
    ++indent;
    pai("std::unique_ptr<Expr> expr = ");
    print_field(*this, a.expr);
    pai("bool suppress = ");
    print_field(*this, a.suppress);
    pai("Maybe<Span const> dot = ");
    print_field(*this, a.dot);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visit(ASTNS::RetStmt &a) {
    pai("RetStmt {\n");
    ++indent;
    pai("std::unique_ptr<Expr> expr = ");
    print_field(*this, a.expr);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visit(ASTNS::PathType &a) {
    pai("PathType {\n");
    ++indent;
    pai("std::unique_ptr<Path> path = ");
    print_field(*this, a.path);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visit(ASTNS::PointerType &a) {
    pai("PointerType {\n");
    ++indent;
    pai("bool mut = ");
    print_field(*this, a.mut);
    pai("std::unique_ptr<Type> type = ");
    print_field(*this, a.type);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visit(ASTNS::ThisType &a) {
    pai("ThisType {\n");
    ++indent;
    pai("Located<Tokens::This> th = ");
    print_field(*this, a.th);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visit(ASTNS::Arg &a) {
    pai("Arg {\n");
    ++indent;
    pai("std::unique_ptr<Expr> expr = ");
    print_field(*this, a.expr);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visit(ASTNS::Param &a) {
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
void ASTNS::PrintVisitor::visit(ASTNS::ThisParam &a) {
    pai("ThisParam {\n");
    ++indent;
    pai("bool ptr = ");
    print_field(*this, a.ptr);
    pai("bool mut = ");
    print_field(*this, a.mut);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visit(ASTNS::Block &a) {
    pai("Block {\n");
    ++indent;
    pai("std::vector<std::unique_ptr<Stmt>> stmts = ");
    print_field(*this, a.stmts);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visit(ASTNS::IfExpr &a) {
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
void ASTNS::PrintVisitor::visit(ASTNS::WhileExpr &a) {
    pai("WhileExpr {\n");
    ++indent;
    pai("std::unique_ptr<Expr> cond = ");
    print_field(*this, a.cond);
    pai("std::unique_ptr<Expr> body = ");
    print_field(*this, a.body);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visit(ASTNS::AssignmentExpr &a) {
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
void ASTNS::PrintVisitor::visit(ASTNS::ShortCircuitExpr &a) {
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
void ASTNS::PrintVisitor::visit(ASTNS::BinaryExpr &a) {
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
void ASTNS::PrintVisitor::visit(ASTNS::CastExpr &a) {
    pai("CastExpr {\n");
    ++indent;
    pai("std::unique_ptr<Type> type = ");
    print_field(*this, a.type);
    pai("std::unique_ptr<Expr> expr = ");
    print_field(*this, a.expr);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visit(ASTNS::UnaryExpr &a) {
    pai("UnaryExpr {\n");
    ++indent;
    pai("Located<UnaryOperator> op = ");
    print_field(*this, a.op);
    pai("std::unique_ptr<Expr> expr = ");
    print_field(*this, a.expr);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visit(ASTNS::AddrofExpr &a) {
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
void ASTNS::PrintVisitor::visit(ASTNS::DerefExpr &a) {
    pai("DerefExpr {\n");
    ++indent;
    pai("Located<Tokens::Star> op = ");
    print_field(*this, a.op);
    pai("std::unique_ptr<Expr> expr = ");
    print_field(*this, a.expr);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visit(ASTNS::CallExpr &a) {
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
void ASTNS::PrintVisitor::visit(ASTNS::FieldAccessExpr &a) {
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
void ASTNS::PrintVisitor::visit(ASTNS::MethodCallExpr &a) {
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
void ASTNS::PrintVisitor::visit(ASTNS::BoolLit &a) {
    pai("BoolLit {\n");
    ++indent;
    pai("Located<Tokens::BoolLit> val = ");
    print_field(*this, a.val);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visit(ASTNS::FloatLit &a) {
    pai("FloatLit {\n");
    ++indent;
    pai("Located<Tokens::FloatLit> val = ");
    print_field(*this, a.val);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visit(ASTNS::IntLit &a) {
    pai("IntLit {\n");
    ++indent;
    pai("Located<Tokens::IntLit> val = ");
    print_field(*this, a.val);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visit(ASTNS::CharLit &a) {
    pai("CharLit {\n");
    ++indent;
    pai("Located<Tokens::CharLit> val = ");
    print_field(*this, a.val);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visit(ASTNS::StringLit &a) {
    pai("StringLit {\n");
    ++indent;
    pai("Located<Tokens::StringLit> val = ");
    print_field(*this, a.val);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visit(ASTNS::ThisExpr &a) {
    pai("ThisExpr {\n");
    ++indent;
    pai("Located<Tokens::This> tok = ");
    print_field(*this, a.tok);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visit(ASTNS::PathExpr &a) {
    pai("PathExpr {\n");
    ++indent;
    pai("std::unique_ptr<Path> path = ");
    print_field(*this, a.path);
    --indent;
    pai("}\n");
}
void ASTNS::PrintVisitor::visit(ASTNS::Path &a) {
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

ASTNS::PrintVisitor::PrintVisitor(llvm::raw_ostream &ostream): indent(0), ostream(ostream) {}
