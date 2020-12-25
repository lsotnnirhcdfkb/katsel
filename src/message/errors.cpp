#include "errors.h"
#include "message/error.h"
#include "message/ansistuff.h"
#include "utils/format.h"
#include <iostream>
#include <cstdlib>
#include <typeinfo>

ErrorFormat errformat = ErrorFormat::HUMAN;
// LocationVisitor {{{1
class LocationVisitor :
    // LOCVISITOR INHERIT START
// The following code was autogenerated - see the utils/ directory
public ASTNS::CUB::Visitor,
public ASTNS::Decl::Visitor,
public ASTNS::Stmt::Visitor,
public ASTNS::Expr::Visitor,
public ASTNS::Type::Visitor,
public ASTNS::ArgB::Visitor,
public ASTNS::ParamB::Visitor,
public ASTNS::VStmtIB::Visitor,
public ASTNS::ImplRetB::Visitor
// This code was autogenerated - see the utils/ directory
    // LOCVISITOR INHERIT END
{
public:
    // LOCVISITOR METHODS START
// The following code was autogenerated - see the utils/ directory
void visitCU(ASTNS::CU *ast) override;
void visitDeclList(ASTNS::DeclList *ast) override;
void visitFunctionDecl(ASTNS::FunctionDecl *ast) override;
void visitVarStmt(ASTNS::VarStmt *ast) override;
void visitVarStmtItem(ASTNS::VarStmtItem *ast) override;
void visitVarStmtItemList(ASTNS::VarStmtItemList *ast) override;
void visitExprStmt(ASTNS::ExprStmt *ast) override;
void visitRetStmt(ASTNS::RetStmt *ast) override;
void visitStmtList(ASTNS::StmtList *ast) override;
void visitImplRet(ASTNS::ImplRet *ast) override;
void visitPrimitiveType(ASTNS::PrimitiveType *ast) override;
void visitArg(ASTNS::Arg *ast) override;
void visitArgList(ASTNS::ArgList *ast) override;
void visitParam(ASTNS::Param *ast) override;
void visitParamList(ASTNS::ParamList *ast) override;
void visitBlock(ASTNS::Block *ast) override;
void visitIfExpr(ASTNS::IfExpr *ast) override;
void visitForExpr(ASTNS::ForExpr *ast) override;
void visitAssignmentExpr(ASTNS::AssignmentExpr *ast) override;
void visitShortCircuitExpr(ASTNS::ShortCircuitExpr *ast) override;
void visitBinaryExpr(ASTNS::BinaryExpr *ast) override;
void visitCastExpr(ASTNS::CastExpr *ast) override;
void visitUnaryExpr(ASTNS::UnaryExpr *ast) override;
void visitCallExpr(ASTNS::CallExpr *ast) override;
void visitPrimaryExpr(ASTNS::PrimaryExpr *ast) override;
// This code was autogenerated - see the utils/ directory
    // LOCVISITOR METHODS END

    template <typename AST>
    std::string::iterator getL(AST *a)
    {
        std::string::iterator cachedr = retr;
        File *cachedf = retf;
        a->accept(this);
        retr = cachedr;
        retf = cachedf;
        return retl;
    }
    template <typename AST>
    std::string::iterator getR(AST *a)
    {
        std::string::iterator cachedl = retl;
        File *cachedf = retf;
        a->accept(this);
        retl = cachedl;
        retf = cachedf;
        return retr;
    }
    template <typename AST>
    File* getF(AST *a)
    {
        std::string::iterator cachedl = retl;
        std::string::iterator cachedr = retr;
        a->accept(this);
        retl = cachedl;
        retr = cachedr;
        return retf;
    }

private:
    std::string::iterator retl;
    std::string::iterator retr;
    File *retf;
};
// location visitor method implementations {{{1
// LOCVISITOR IMPL START
// The following code was autogenerated - see the utils/ directory
void LocationVisitor::visitCU(ASTNS::CU *ast)
{
            retl = getL(ast->decls.get());
            retf = getF(ast->decls.get());
            retr = getR(ast->decls.get());
}
void LocationVisitor::visitDeclList(ASTNS::DeclList *ast)
{
            retl = ast->decls.start;
            retf = ast->decls.sourcefile;
            retr = ast->decls.end;
}
void LocationVisitor::visitFunctionDecl(ASTNS::FunctionDecl *ast)
{
            retl = getL(ast->retty.get());
            retf = getF(ast->retty.get());
            retr = getR(ast->body.get());
}
void LocationVisitor::visitVarStmt(ASTNS::VarStmt *ast)
{
            retl = getL(ast->type.get());
            retf = getF(ast->type.get());
            retr = getR(ast->assignments.get());
}
void LocationVisitor::visitVarStmtItem(ASTNS::VarStmtItem *ast)
{
            retl = ast->name.start;
            retf = ast->name.sourcefile;
            retr = getR(ast->expr.get());
}
void LocationVisitor::visitVarStmtItemList(ASTNS::VarStmtItemList *ast)
{
            retl = ast->items.start;
            retf = ast->items.sourcefile;
            retr = ast->items.end;
}
void LocationVisitor::visitExprStmt(ASTNS::ExprStmt *ast)
{
            retl = getL(ast->expr.get());
            retf = getF(ast->expr.get());
            retr = getR(ast->expr.get());
}
void LocationVisitor::visitRetStmt(ASTNS::RetStmt *ast)
{
            retl = getL(ast->expr.get());
            retf = getF(ast->expr.get());
            retr = getR(ast->expr.get());
}
void LocationVisitor::visitStmtList(ASTNS::StmtList *ast)
{
            retl = ast->stmts.start;
            retf = ast->stmts.sourcefile;
            retr = ast->stmts.end;
}
void LocationVisitor::visitImplRet(ASTNS::ImplRet *ast)
{
            retl = getL(ast->expr.get());
            retf = getF(ast->expr.get());
            retr = getR(ast->expr.get());
}
void LocationVisitor::visitPrimitiveType(ASTNS::PrimitiveType *ast)
{
            retl = ast->ty.start;
            retf = ast->ty.sourcefile;
            retr = ast->ty.end;
}
void LocationVisitor::visitArg(ASTNS::Arg *ast)
{
            retl = getL(ast->expr.get());
            retf = getF(ast->expr.get());
            retr = getR(ast->expr.get());
}
void LocationVisitor::visitArgList(ASTNS::ArgList *ast)
{
            retl = ast->args.start;
            retf = ast->args.sourcefile;
            retr = ast->args.end;
}
void LocationVisitor::visitParam(ASTNS::Param *ast)
{
            retl = getL(ast->ty.get());
            retf = getF(ast->ty.get());
            retr = ast->name.end;
}
void LocationVisitor::visitParamList(ASTNS::ParamList *ast)
{
            retl = ast->params.start;
            retf = ast->params.sourcefile;
            retr = ast->params.end;
}
void LocationVisitor::visitBlock(ASTNS::Block *ast)
{
            retl = getL(ast->stmts.get());
            retf = getF(ast->stmts.get());
            retr = getR(ast->implRet.get());
}
void LocationVisitor::visitIfExpr(ASTNS::IfExpr *ast)
{
            retl = getL(ast->cond.get());
            retf = getF(ast->cond.get());
            retr = getR(ast->falses.get());
}
void LocationVisitor::visitForExpr(ASTNS::ForExpr *ast)
{
            retl = getL(ast->start.get());
            retf = getF(ast->start.get());
            retr = getR(ast->body.get());
}
void LocationVisitor::visitAssignmentExpr(ASTNS::AssignmentExpr *ast)
{
            retl = getL(ast->target.get());
            retf = getF(ast->target.get());
            retr = getR(ast->expr.get());
}
void LocationVisitor::visitShortCircuitExpr(ASTNS::ShortCircuitExpr *ast)
{
            retl = getL(ast->lhs.get());
            retf = getF(ast->lhs.get());
            retr = getR(ast->rhs.get());
}
void LocationVisitor::visitBinaryExpr(ASTNS::BinaryExpr *ast)
{
            retl = getL(ast->lhs.get());
            retf = getF(ast->lhs.get());
            retr = getR(ast->rhs.get());
}
void LocationVisitor::visitCastExpr(ASTNS::CastExpr *ast)
{
            retl = getL(ast->castto.get());
            retf = getF(ast->castto.get());
            retr = getR(ast->expr.get());
}
void LocationVisitor::visitUnaryExpr(ASTNS::UnaryExpr *ast)
{
            retl = ast->op.start;
            retf = ast->op.sourcefile;
            retr = getR(ast->expr.get());
}
void LocationVisitor::visitCallExpr(ASTNS::CallExpr *ast)
{
            retl = getL(ast->callee.get());
            retf = getF(ast->callee.get());
            retr = getR(ast->args.get());
}
void LocationVisitor::visitPrimaryExpr(ASTNS::PrimaryExpr *ast)
{
            retl = ast->value.start;
            retf = ast->value.sourcefile;
            retr = ast->value.end;
}
// This code was autogenerated - see the utils/ directory
// LOCVISITOR IMPL END
// constructors for location {{{1
Location::Location(Token const &t): start(t.start), end(t.end), file(t.sourcefile) {}
Location::Location(IR::ASTValue const &v): Location(v.ast) {}
Location::Location(IR::ASTValue const *v): Location(v->ast) {}
Location::Location(std::string::iterator start, std::string::iterator end, File const *file): start(start), end(end), file(file) {}

Location::Location(ASTNS::AST *ast)
{
#define CHECKTY(ty) \
    ASTNS::ty *casted ## ty; \
    if ((casted ## ty = dynamic_cast<ASTNS::ty*>(ast))) \
    { \
        LocationVisitor locV; \
        start = locV.getL(casted ## ty); \
        end = locV.getR(casted ## ty); \
        file = locV.getF(casted ## ty); \
        return; \
    }
    CHECKTY(DeclB)
    CHECKTY(ArgB)
    CHECKTY(StmtB)
    CHECKTY(ExprB)
    CHECKTY(VStmtIB)
    CHECKTY(PListB)
    CHECKTY(TypeB)
#undef CHECKTY
    if (!ast)
        reportAbortNoh("Location constructor called with nullptr ast");
    else
        reportAbortNoh(format("Location constructor reached invalid ast type: %", typeid(ast).name()));
}
// Error methods {{{1
Error::Error(MsgType type, Location const &location, std::string message): type(type), location(location), message(message) {}
Error& Error::underline(Underline const &underline)
{
    underlines.push_back(underline);
    return *this;
}
// Underline message methods {{{1
Error::Underline::Underline(Location const &location, char ch): location(location), ch(ch) {}
Error::Underline& Error::Underline::error(std::string const &message)
{
    return addmsg("error", A_FG_RED, message);
}
Error::Underline& Error::Underline::warning(std::string const &message)
{
    return addmsg("warning", A_FG_MAGENTA, message);
}
Error::Underline& Error::Underline::note(std::string const &message)
{
    return addmsg("note", A_FG_GREEN, message);
}
Error::Underline& Error::Underline::help(std::string const &message)
{
    return addmsg("help", A_FG_CYAN, message);
}
Error::Underline& Error::Underline::hint(std::string const &message)
{
    return addmsg("hint", A_FG_YELLOW, message);
}
Error::Underline& Error::Underline::message(std::string const &type, std::string const &message)
{
    return addmsg(type, A_FG_WHITE A_BOLD, message);
}
Error::Underline& Error::Underline::addmsg(std::string const &type, char const * const color, std::string const &message)
{
    messages.push_back(Message {type, message, color});
    return *this;
}
// other internal errors {{{1
void reportAbortNoh(std::string const &message)
{
    std::cerr << "!!! Unrecoverable brokenness discovered in compiler !!!: " << message << std::endl;
    std::cerr << "!!! this is a bug - whether or not it has a bug report is unknown" << std::endl;
    std::cerr << "!!! bugs can be reported on the Katsel GitHub page: https://github.com/hpj2ltxry43b/katsel/issues" << std::endl;
    std::cerr << "!!! please search far and wide (on the GitHub page) before reporting a bug, so that there are no duplicate bug reports!" << std::endl;
    std::cerr << "Aborting..." << std::endl;
    std::abort();
}
void invalidTok(std::string const &name, Token const &underline)
{
    reportAbortNoh(format("invalid token for %: \"%\"", name, underline));
}
void calledWithOpTyNEthis(std::string const &classN, std::string const &fnn, std::string const &opname)
{
    reportAbortNoh(format("%::% called with % type != this", classN, fnn, opname));
}
void outOSwitchDDefaultLab(std::string const &fnn, Location const &highlight)
{
    reportAbortNoh(format("% went out of switch despite default label", fnn));
}
void fCalled(std::string const &fnn)
{
    reportAbortNoh(format("% called", fnn));
}
void outOSwitchNoh(std::string const &fnn)
{
    reportAbortNoh(format("% went out of switch", fnn));
}
