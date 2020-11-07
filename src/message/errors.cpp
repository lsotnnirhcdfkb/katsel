#include "message/errors.h"
#include "visit/visitor.h"
#include "message/ansistuff.h"
#include <iostream>
#include <cstdlib>
#include <sstream>

// getLine {{{1
Location getLine(Location const &l)
{
    auto linestart (l.start);
    while (linestart != l.file->source.begin() && *linestart != '\n') --linestart; // until *linestart is \n
    // once *linestart is \n then go forward once

    // if linestart == l.file.source.begin(), then loop stopped
    // because iterator hit beginning of sourcefile.source, not
    // because it hit \n, so there is no need to consume the \n
    if (linestart != l.file->source.begin())
        ++linestart;

    auto lineend (l.end);
    while (lineend != l.file->source.end() && *lineend != '\n') ++lineend;
    // *lineend should be \n

    return Location(linestart, lineend, l.file);
}
// getColN {{{1
int getColN(std::string::const_iterator const &start, std::string::iterator loc)
{
    int coln = 1;
    --loc;

    for (; loc != start && *loc != '\n'; ++coln, --loc)
        ;

    if (loc != start)
        ++loc, --coln;

    return coln + 1;
}
// getLineN {{{1
int getLineN(std::string::const_iterator const &start, std::string::iterator loc)
{
    int linen = 0;
    while (loc != start)
    {
        if (*loc == '\n') ++linen;
        --loc;
    }
    return linen;
}
// LocationVisitor {{{1
class LocationVisitor :
    public ExprVisitor,
    public DeclVisitor,
    public TypeVisitor,
    public StmtVisitor
{
    void visitBinaryExpr(ASTNS::BinaryExpr *a)
    {
        retl = getL(a->lhs.get());
        retr = getR(a->rhs.get());
        retf = getF(a->lhs.get());
    }
    void visitTernaryExpr(ASTNS::TernaryExpr *a)
    {
        retl = getL(a->condition.get());
        retr = getR(a->falses.get());
        retf = getF(a->falses.get());
    }
    void visitUnaryExpr(ASTNS::UnaryExpr *a)
    {
        retl = a->op.start;
        retr = getR(a->operand.get());
        retf = a->op.sourcefile;
    }
    void visitPrimaryExpr(ASTNS::PrimaryExpr *a)
    {
        retl = a->value.start;
        retr = a->value.end;
        retf = a->value.sourcefile;
    }
    void visitCallExpr(ASTNS::CallExpr *a)
    {
        retl = getL(a->func.get());
        retr = getR(a->func.get()); // TODO: get closing paren token
        retf = getF(a->func.get());
    }
    void visitFunctionDecl(ASTNS::FunctionDecl *a)
    {
        retl = a->name.start; // TODO: get fun token
        retr = getR(a->block.get());
        retf = a->name.sourcefile;
    }
    void visitGlobalVarDecl(ASTNS::GlobalVarDecl *a)
    {
        retl = a->name.start; // TODO
        retr = getR(a->value.get());
        retf = a->name.sourcefile;
    }
    void visitBaseType(ASTNS::BaseType *a)
    {
        retl = a->type.start;
        retr = a->type.end;
        retf = a->type.sourcefile;
    }
    void visitBlockStmt(ASTNS::BlockStmt *a)
    {
        retl = getL(a->stmts[0].get()); // TODO
        retr = getR(a->stmts[a->stmts.size() - 1].get()); // TODO
        retf = getF(a->stmts[0].get());
    }
    void visitExprStmt(ASTNS::ExprStmt *a)
    {
        retl = getL(a->expr.get());
        retr = getR(a->expr.get());
        retf = getF(a->expr.get());
    }
    void visitReturnStmt(ASTNS::ReturnStmt *a)
    {
        retl = getL(a->val.get()); // TODO
        retr = getR(a->val.get());
        retf = getF(a->val.get());
    }
    void visitVarStmt(ASTNS::VarStmt *a)
    {
        retl = a->name.start; // TODO
        retr = getR(a->assign.get());
        retf = a->name.sourcefile;
    }

public:
    template <typename AST>
    std::string::iterator getL(AST *a)
    {
        a->accept(this);
        return retl;

    }
    template <typename AST>
    std::string::iterator getR(AST *a)
    {
        a->accept(this);
        return retr;
    }
    template <typename AST>
    File* getF(AST *a)
    {
        a->accept(this);
        return retf;
    }

private:
    std::string::iterator retl;
    std::string::iterator retr;
    File *retf;
};

// constructors for location {{{1
Location::Location(Token const &t): start(t.start), end(t.end), file(t.sourcefile) {}
Location::Location(Value const &v): Location(v.ast) {}
Location::Location(std::string::iterator start, std::string::iterator end, File const *file): start(start), end(end), file(file) {}

Location::Location(ASTNS::Expr *a)
{
    LocationVisitor locV;
    start = locV.getL(a);
    end = locV.getR(a);
    file = locV.getF(a);
}
Location::Location(ASTNS::Decl *a)
{
    LocationVisitor locV;
    start = locV.getL(a);
    end = locV.getR(a);
    file = locV.getF(a);
}
Location::Location(ASTNS::Type *a)
{
    LocationVisitor locV;
    start = locV.getL(a);
    end = locV.getR(a);
    file = locV.getF(a);
}
Location::Location(ASTNS::Stmt *a)
{
    LocationVisitor locV;
    start = locV.getL(a);
    end = locV.getR(a);
    file = locV.getF(a);
}
// Error methods {{{1
Error& Error::primary(Primary const &primary)
{
    int l = getLineN(primary.location.file->source.begin(), primary.location.start);
    for (Primary const &p : primaries)
        if (getLineN(p.location.file->source.begin(), p.location.start) == l)
            std::abort(); // TODO: internal error

    primaries.push_back(primary);
    return *this;
}
Error& Error::secondary(Location const &location)
{
    secondaries.push_back(location);
    return *this;
}
Error& Error::span(Location const &start, Location const &end)
{
    spans.push_back(Span {start, end});
    return *this;
}

// Error report method {{{1
void Error::report()
{
    std::cerr << "Error" << std::endl;
}
// Primary message methods {{{1
Error::Primary::Primary(Location const &location): location(location) { }
Error::Primary& Error::Primary::error(std::string const &message)
{
    return addmsg("error", A_FG_RED, message);
}
Error::Primary& Error::Primary::warning(std::string const &message)
{
    return addmsg("warning", A_FG_MAGENTA, message);
}
Error::Primary& Error::Primary::note(std::string const &message)
{
    return addmsg("note", A_FG_GREEN, message);
}
Error::Primary& Error::Primary::help(std::string const &message)
{
    return addmsg("help", A_FG_CYAN, message);
}
Error::Primary& Error::Primary::hint(std::string const &message)
{
    return addmsg("hint", A_FG_YELLOW, message);
}
Error::Primary& Error::Primary::message(std::string const &type, std::string const &message)
{
    return addmsg(type, A_FG_WHITE A_BOLD, message);
}
Error::Primary& Error::Primary::addmsg(std::string const &type, char const * const color, std::string const &message)
{
    messages.push_back(Message {type, message, color});
    return *this;
}
