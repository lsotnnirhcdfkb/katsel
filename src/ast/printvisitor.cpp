#include "ast/printvisitor.h"

#include <iostream>
#include <string>

// PRINTVISITOR START
// The following code was autogenerated - see the utils/ directory
void ASTNS::PrintVisitor::visitPureLocation(ASTNS::PureLocation *a)
{
    pai("PureLocation\n");
    ++indent;
    pai("int dummy = ");
    printField(a->dummy);
    --indent;
}
void ASTNS::PrintVisitor::visitCU(ASTNS::CU *a)
{
    pai("CU\n");
    ++indent;
    pai("std::unique_ptr<DeclList> decls = ");
    printField(a->decls);
    --indent;
}
void ASTNS::PrintVisitor::visitDeclList(ASTNS::DeclList *a)
{
    pai("DeclList\n");
    ++indent;
    pai("std::vector<std::unique_ptr<Decl>> decls = ");
    printField(a->decls);
    --indent;
}
void ASTNS::PrintVisitor::visitFunctionDecl(ASTNS::FunctionDecl *a)
{
    pai("FunctionDecl\n");
    ++indent;
    pai("std::unique_ptr<Type> retty = ");
    printField(a->retty);
    pai("Token name = ");
    printField(a->name);
    pai("std::unique_ptr<ParamList> params = ");
    printField(a->params);
    pai("std::unique_ptr<Block> body = ");
    printField(a->body);
    --indent;
}
void ASTNS::PrintVisitor::visitVarStmt(ASTNS::VarStmt *a)
{
    pai("VarStmt\n");
    ++indent;
    pai("std::unique_ptr<Type> type = ");
    printField(a->type);
    pai("std::unique_ptr<VarStmtItemList> assignments = ");
    printField(a->assignments);
    --indent;
}
void ASTNS::PrintVisitor::visitVarStmtItem(ASTNS::VarStmtItem *a)
{
    pai("VarStmtItem\n");
    ++indent;
    pai("Token name = ");
    printField(a->name);
    pai("Token equal = ");
    printField(a->equal);
    pai("std::unique_ptr<Expr> expr = ");
    printField(a->expr);
    --indent;
}
void ASTNS::PrintVisitor::visitVarStmtItemList(ASTNS::VarStmtItemList *a)
{
    pai("VarStmtItemList\n");
    ++indent;
    pai("std::vector<std::unique_ptr<VarStmtItem>> items = ");
    printField(a->items);
    --indent;
}
void ASTNS::PrintVisitor::visitExprStmt(ASTNS::ExprStmt *a)
{
    pai("ExprStmt\n");
    ++indent;
    pai("std::unique_ptr<Expr> expr = ");
    printField(a->expr);
    --indent;
}
void ASTNS::PrintVisitor::visitRetStmt(ASTNS::RetStmt *a)
{
    pai("RetStmt\n");
    ++indent;
    pai("std::unique_ptr<Expr> expr = ");
    printField(a->expr);
    --indent;
}
void ASTNS::PrintVisitor::visitStmtList(ASTNS::StmtList *a)
{
    pai("StmtList\n");
    ++indent;
    pai("std::vector<std::unique_ptr<Stmt>> stmts = ");
    printField(a->stmts);
    --indent;
}
void ASTNS::PrintVisitor::visitImplRet(ASTNS::ImplRet *a)
{
    pai("ImplRet\n");
    ++indent;
    pai("std::unique_ptr<Expr> expr = ");
    printField(a->expr);
    --indent;
}
void ASTNS::PrintVisitor::visitPrimitiveType(ASTNS::PrimitiveType *a)
{
    pai("PrimitiveType\n");
    ++indent;
    pai("Token ty = ");
    printField(a->ty);
    --indent;
}
void ASTNS::PrintVisitor::visitPointerType(ASTNS::PointerType *a)
{
    pai("PointerType\n");
    ++indent;
    pai("std::unique_ptr<Type> type = ");
    printField(a->type);
    --indent;
}
void ASTNS::PrintVisitor::visitArg(ASTNS::Arg *a)
{
    pai("Arg\n");
    ++indent;
    pai("std::unique_ptr<Expr> expr = ");
    printField(a->expr);
    --indent;
}
void ASTNS::PrintVisitor::visitArgList(ASTNS::ArgList *a)
{
    pai("ArgList\n");
    ++indent;
    pai("std::vector<std::unique_ptr<Arg>> args = ");
    printField(a->args);
    --indent;
}
void ASTNS::PrintVisitor::visitParam(ASTNS::Param *a)
{
    pai("Param\n");
    ++indent;
    pai("std::unique_ptr<Type> type = ");
    printField(a->type);
    pai("Token name = ");
    printField(a->name);
    --indent;
}
void ASTNS::PrintVisitor::visitParamList(ASTNS::ParamList *a)
{
    pai("ParamList\n");
    ++indent;
    pai("std::vector<std::unique_ptr<Param>> params = ");
    printField(a->params);
    --indent;
}
void ASTNS::PrintVisitor::visitBlock(ASTNS::Block *a)
{
    pai("Block\n");
    ++indent;
    pai("std::unique_ptr<StmtList> stmts = ");
    printField(a->stmts);
    pai("std::unique_ptr<ImplRet> implRet = ");
    printField(a->implRet);
    --indent;
}
void ASTNS::PrintVisitor::visitIfExpr(ASTNS::IfExpr *a)
{
    pai("IfExpr\n");
    ++indent;
    pai("Token iftok = ");
    printField(a->iftok);
    pai("std::unique_ptr<Expr> cond = ");
    printField(a->cond);
    pai("std::unique_ptr<Expr> trues = ");
    printField(a->trues);
    pai("std::unique_ptr<Expr> falses = ");
    printField(a->falses);
    --indent;
}
void ASTNS::PrintVisitor::visitForExpr(ASTNS::ForExpr *a)
{
    pai("ForExpr\n");
    ++indent;
    pai("std::unique_ptr<VarStmt> initial = ");
    printField(a->initial);
    pai("std::unique_ptr<Expr> cond = ");
    printField(a->cond);
    pai("std::unique_ptr<Expr> increment = ");
    printField(a->increment);
    pai("std::unique_ptr<Expr> body = ");
    printField(a->body);
    --indent;
}
void ASTNS::PrintVisitor::visitAssignmentExpr(ASTNS::AssignmentExpr *a)
{
    pai("AssignmentExpr\n");
    ++indent;
    pai("std::unique_ptr<Expr> target = ");
    printField(a->target);
    pai("Token equal = ");
    printField(a->equal);
    pai("std::unique_ptr<Expr> expr = ");
    printField(a->expr);
    --indent;
}
void ASTNS::PrintVisitor::visitShortCircuitExpr(ASTNS::ShortCircuitExpr *a)
{
    pai("ShortCircuitExpr\n");
    ++indent;
    pai("std::unique_ptr<Expr> lhs = ");
    printField(a->lhs);
    pai("Token op = ");
    printField(a->op);
    pai("std::unique_ptr<Expr> rhs = ");
    printField(a->rhs);
    --indent;
}
void ASTNS::PrintVisitor::visitBinaryExpr(ASTNS::BinaryExpr *a)
{
    pai("BinaryExpr\n");
    ++indent;
    pai("std::unique_ptr<Expr> lhs = ");
    printField(a->lhs);
    pai("Token op = ");
    printField(a->op);
    pai("std::unique_ptr<Expr> rhs = ");
    printField(a->rhs);
    --indent;
}
void ASTNS::PrintVisitor::visitCastExpr(ASTNS::CastExpr *a)
{
    pai("CastExpr\n");
    ++indent;
    pai("std::unique_ptr<Type> type = ");
    printField(a->type);
    pai("std::unique_ptr<Expr> expr = ");
    printField(a->expr);
    --indent;
}
void ASTNS::PrintVisitor::visitUnaryExpr(ASTNS::UnaryExpr *a)
{
    pai("UnaryExpr\n");
    ++indent;
    pai("Token op = ");
    printField(a->op);
    pai("std::unique_ptr<Expr> expr = ");
    printField(a->expr);
    --indent;
}
void ASTNS::PrintVisitor::visitCallExpr(ASTNS::CallExpr *a)
{
    pai("CallExpr\n");
    ++indent;
    pai("std::unique_ptr<Expr> callee = ");
    printField(a->callee);
    pai("Token oparn = ");
    printField(a->oparn);
    pai("std::unique_ptr<ArgList> args = ");
    printField(a->args);
    --indent;
}
void ASTNS::PrintVisitor::visitPrimaryExpr(ASTNS::PrimaryExpr *a)
{
    pai("PrimaryExpr\n");
    ++indent;
    pai("Token value = ");
    printField(a->value);
    --indent;
}
// This code was autogenerated - see the utils/ directory
// PRINTVISITOR END

void ASTNS::PrintVisitor::pai(std::string const &s)
{
    for (auto i = s.begin(); i != s.end(); ++i)
    {
        if (pindent)
            for (int j = 0; j < indent; ++j)
                ostream << "  ";

        pindent = false;
        ostream << *i;

        if (*i == '\n')
            pindent = true;
    }
}

ASTNS::PrintVisitor::PrintVisitor(llvm::raw_ostream &ostream): indent(0), ostream(ostream) {}

void ASTNS::PrintVisitor::printField(Token const &t)
{
    pai("\"");
    pai(t.stringify());
    pai("\"\n");
}
void ASTNS::PrintVisitor::printField(int i)
{
    ostream << i << '\n';
}
