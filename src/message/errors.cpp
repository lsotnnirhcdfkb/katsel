#include "message/errors.h"
#include "message/ansistuff.h"
#include "visit/visitor.h"

#include <iostream>
#include <vector>
#include <algorithm>

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
// report {{{1
template <typename ... Locations>
void report(MsgType msgtype, const std::string &message, Location shl, Locations ... l)
{
    std::vector<Location> underlines (l...);
    Location showl (getLine(shl));

    // switch msgtype {{{
    switch (msgtype)
    {
        case MsgType::ERROR:
            if (ansiCodesEnabled())
                std::cerr << A_BOLD A_FG_RED "Error" A_RESET " in " A_FG_CYAN << showl.file->filename << A_RESET " " << message << std::endl;
            else
                std::cerr << "Error in " << showl.file->filename << " " << message << std::endl;
            break;
        case MsgType::WARNING:
            if (ansiCodesEnabled())
                std::cerr << A_BOLD A_FG_MAGENTA "Warning" A_RESET " in " A_FG_CYAN << showl.file->filename << A_RESET " " << message << std::endl;
            else
                std::cerr << "Warning in " << showl.file->filename << " " << message << std::endl;
            break;
        case MsgType::DEBUG:
            if (ansiCodesEnabled())
                std::cerr << A_BOLD A_FG_GREEN "Debug message" A_RESET " in " A_FG_CYAN << showl.file->filename << A_RESET " " << message << std::endl;
            else
                std::cerr << "Debug message in " << showl.file->filename << " " << message << std::endl;
            break;
        case MsgType::INTERNALERR:
            if (ansiCodesEnabled())
                std::cerr << A_BOLD "!!! - " A_FG_RED "Internal error" A_RESET ": " A_BOLD << message << A_RESET << std::endl;
            else
                std::cerr << "!!! - Internal error: " << message << std::endl;
            std::cerr << "Aborting" << std::endl;
            std::abort();
            break;
    }
    // }}}

    if (ansiCodesEnabled())
        std::cerr << A_FG_CYAN;

    auto pl = showl.start;
    auto ul = showl.start;
    while (true)
    {
        while (*pl != '\n' && pl != showl.end)
        {
            std::cout << *pl;
            ++pl;
        }

        while (*ul != '\n' && ul != showl.end)
        {
            bool draw = false;
            for (Location &l : underlines)
            {
                if (ul >= l.start && ul < l.end)
                {
                    draw = true;
                    break;
                }
            }

            if (draw)
                std::cerr << "^";
            else
                std::cerr << " ";
            ++ul;
        }

        if (pl == showl.end)
            break;

        ++pl; // consume \n
        ++ul;
    }

    if (ansiCodesEnabled())
        std::cerr << A_RESET;
    std::cerr << std::endl;
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
        retf = &a->op.sourcefile;
    }
    void visitPrimaryExpr(ASTNS::PrimaryExpr *a)
    {
        retl = a->value.start;
        retr = a->value.end;
        retf = &a->value.sourcefile;
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
        retf = &a->name.sourcefile;
    }
    void visitGlobalVarDecl(ASTNS::GlobalVarDecl *a)
    {
        retl = a->name.start; // TODO
        retr = getR(a->value.get());
        retf = &a->name.sourcefile;
    }
    void visitBaseType(ASTNS::BaseType *a)
    {
        retl = a->type.start;
        retr = a->type.end;
        retf = &a->type.sourcefile;
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
        retf = &a->name.sourcefile;
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
Location::Location(Token &t): start(t.start), end(t.end), file(&t.sourcefile) {}
Location::Location(std::string::iterator start, std::string::iterator end, File *file): start(start), end(end), file(file) {}

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
