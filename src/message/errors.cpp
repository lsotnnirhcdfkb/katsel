#include "message/errors.h"
#include "ast/visitor.h"
#include "message/ansistuff.h"
#include <iostream>
#include <iomanip>
#include <cstdlib>
#include <algorithm>
#include <typeinfo>

// getLine {{{1
void getLine(std::string::const_iterator &lstarto, std::string::const_iterator &lendo, File const &f, int linenr)
{
    int cline = linenr;
    std::string::const_iterator lstart = f.source.begin();
    for (; lstart < f.source.end() && cline > 1; ++lstart)
        if (*lstart == '\n')
            --cline;

    if (lstart == f.source.end())
    {
        lstarto = lendo = lstart;
    }

    auto lend (lstart);
    while (*lend != '\n' && lend != f.source.end())
        ++lend;

    lstarto = lstart;
    lendo = lend;
}
// getColN {{{1
int getColN(std::string::const_iterator const &start, std::string::const_iterator loc)
{
    int coln = 1;

    for (; loc != start && *loc != '\n'; ++coln, --loc)
        ;

    if (loc != start)
        ++loc, --coln;

    return coln;
}
// getLineN {{{1
int getLineN(std::string::const_iterator const &start, std::string::iterator loc)
{
    int linen = 0;
    while (loc >= start)
    {
        if (*loc == '\n') ++linen;
        --loc;
    }
    return linen + 1;
}
// LocationVisitor {{{1
class LocationVisitor :
    public ASTNS::DeclBVisitor,
    public ASTNS::ArgBVisitor,
    public ASTNS::StmtBVisitor,
    public ASTNS::ExprBVisitor,
    public ASTNS::VStmtIBVisitor,
    public ASTNS::PListBVisitor,
    public ASTNS::TypeBVisitor
{
public:
    // LOCVISITOR METHODS START

// The following code was autogenerated - see the utils/ directory
void visitAdditionExpr(ASTNS::AdditionExpr *ast) override;
void visitArg(ASTNS::Arg *ast) override;
void visitArgList(ASTNS::ArgList *ast) override;
void visitAssignmentExpr(ASTNS::AssignmentExpr *ast) override;
void visitBinandExpr(ASTNS::BinandExpr *ast) override;
void visitBinnotExpr(ASTNS::BinnotExpr *ast) override;
void visitBinorExpr(ASTNS::BinorExpr *ast) override;
void visitBitandExpr(ASTNS::BitandExpr *ast) override;
void visitBitorExpr(ASTNS::BitorExpr *ast) override;
void visitBitshiftExpr(ASTNS::BitshiftExpr *ast) override;
void visitBitxorExpr(ASTNS::BitxorExpr *ast) override;
void visitBlock(ASTNS::Block *ast) override;
void visitBuiltinTypeNoVoid(ASTNS::BuiltinTypeNoVoid *ast) override;
void visitBuiltinTypeVoid(ASTNS::BuiltinTypeVoid *ast) override;
void visitCallExpr(ASTNS::CallExpr *ast) override;
void visitCompeqExpr(ASTNS::CompeqExpr *ast) override;
void visitComplgtExpr(ASTNS::ComplgtExpr *ast) override;
void visitDeclList(ASTNS::DeclList *ast) override;
void visitEmptyStmt(ASTNS::EmptyStmt *ast) override;
void visitExprStmt(ASTNS::ExprStmt *ast) override;
void visitFunction(ASTNS::Function *ast) override;
void visitMultExpr(ASTNS::MultExpr *ast) override;
void visitParam(ASTNS::Param *ast) override;
void visitParamList(ASTNS::ParamList *ast) override;
void visitPrimaryExpr(ASTNS::PrimaryExpr *ast) override;
void visitRetStmt(ASTNS::RetStmt *ast) override;
void visitStmtList(ASTNS::StmtList *ast) override;
void visitTernaryExpr(ASTNS::TernaryExpr *ast) override;
void visitUnaryExpr(ASTNS::UnaryExpr *ast) override;
void visitVarStmt(ASTNS::VarStmt *ast) override;
void visitVarStmtItem(ASTNS::VarStmtItem *ast) override;
void visitVarStmtItemList(ASTNS::VarStmtItemList *ast) override;
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
void LocationVisitor::visitAdditionExpr(ASTNS::AdditionExpr *ast)
{
    switch (ast->form)
    {
        case ASTNS::AdditionExpr::Form::ATA:
            retl = getL(ast->lhs.get());
            retf = getF(ast->lhs.get());
            retr = getR(ast->rhs.get());
            break;
    }
}
void LocationVisitor::visitArg(ASTNS::Arg *ast)
{
    switch (ast->form)
    {
        case ASTNS::Arg::Form::A:
            retl = getL(ast->expr.get());
            retf = getF(ast->expr.get());
            retr = getR(ast->expr.get());
            break;
    }
}
void LocationVisitor::visitArgList(ASTNS::ArgList *ast)
{
    switch (ast->form)
    {
        case ASTNS::ArgList::Form::ATA:
            retl = getL(ast->arglist.get());
            retf = getF(ast->arglist.get());
            retr = getR(ast->arg.get());
            break;
    }
}
void LocationVisitor::visitAssignmentExpr(ASTNS::AssignmentExpr *ast)
{
    switch (ast->form)
    {
        case ASTNS::AssignmentExpr::Form::ATA:
            retl = getL(ast->target.get());
            retf = getF(ast->target.get());
            retr = getR(ast->value.get());
            break;
    }
}
void LocationVisitor::visitBinandExpr(ASTNS::BinandExpr *ast)
{
    switch (ast->form)
    {
        case ASTNS::BinandExpr::Form::ATA:
            retl = getL(ast->lhs.get());
            retf = getF(ast->lhs.get());
            retr = getR(ast->rhs.get());
            break;
    }
}
void LocationVisitor::visitBinnotExpr(ASTNS::BinnotExpr *ast)
{
    switch (ast->form)
    {
        case ASTNS::BinnotExpr::Form::TA:
            retl = ast->op.start;
            retf = ast->op.sourcefile;
            retr = getR(ast->operand.get());
            break;
    }
}
void LocationVisitor::visitBinorExpr(ASTNS::BinorExpr *ast)
{
    switch (ast->form)
    {
        case ASTNS::BinorExpr::Form::ATA:
            retl = getL(ast->lhs.get());
            retf = getF(ast->lhs.get());
            retr = getR(ast->rhs.get());
            break;
    }
}
void LocationVisitor::visitBitandExpr(ASTNS::BitandExpr *ast)
{
    switch (ast->form)
    {
        case ASTNS::BitandExpr::Form::ATA:
            retl = getL(ast->lhs.get());
            retf = getF(ast->lhs.get());
            retr = getR(ast->rhs.get());
            break;
    }
}
void LocationVisitor::visitBitorExpr(ASTNS::BitorExpr *ast)
{
    switch (ast->form)
    {
        case ASTNS::BitorExpr::Form::ATA:
            retl = getL(ast->lhs.get());
            retf = getF(ast->lhs.get());
            retr = getR(ast->rhs.get());
            break;
    }
}
void LocationVisitor::visitBitshiftExpr(ASTNS::BitshiftExpr *ast)
{
    switch (ast->form)
    {
        case ASTNS::BitshiftExpr::Form::ATA:
            retl = getL(ast->lhs.get());
            retf = getF(ast->lhs.get());
            retr = getR(ast->rhs.get());
            break;
    }
}
void LocationVisitor::visitBitxorExpr(ASTNS::BitxorExpr *ast)
{
    switch (ast->form)
    {
        case ASTNS::BitxorExpr::Form::ATA:
            retl = getL(ast->lhs.get());
            retf = getF(ast->lhs.get());
            retr = getR(ast->rhs.get());
            break;
    }
}
void LocationVisitor::visitBlock(ASTNS::Block *ast)
{
    switch (ast->form)
    {
        case ASTNS::Block::Form::TAT:
            retl = ast->ocurb.start;
            retf = ast->ocurb.sourcefile;
            retr = ast->ccurb.end;
            break;
        case ASTNS::Block::Form::TT:
            retl = ast->ocurb.start;
            retf = ast->ocurb.sourcefile;
            retr = ast->ccurb.end;
            break;
    }
}
void LocationVisitor::visitBuiltinTypeNoVoid(ASTNS::BuiltinTypeNoVoid *ast)
{
    switch (ast->form)
    {
        case ASTNS::BuiltinTypeNoVoid::Form::T:
            retl = ast->type.start;
            retf = ast->type.sourcefile;
            retr = ast->type.end;
            break;
    }
}
void LocationVisitor::visitBuiltinTypeVoid(ASTNS::BuiltinTypeVoid *ast)
{
    switch (ast->form)
    {
        case ASTNS::BuiltinTypeVoid::Form::T:
            retl = ast->type.start;
            retf = ast->type.sourcefile;
            retr = ast->type.end;
            break;
    }
}
void LocationVisitor::visitCallExpr(ASTNS::CallExpr *ast)
{
    switch (ast->form)
    {
        case ASTNS::CallExpr::Form::ATAT:
            retl = getL(ast->callee.get());
            retf = getF(ast->callee.get());
            retr = ast->cparn.end;
            break;
        case ASTNS::CallExpr::Form::ATT:
            retl = getL(ast->callee.get());
            retf = getF(ast->callee.get());
            retr = ast->cparn.end;
            break;
    }
}
void LocationVisitor::visitCompeqExpr(ASTNS::CompeqExpr *ast)
{
    switch (ast->form)
    {
        case ASTNS::CompeqExpr::Form::ATA:
            retl = getL(ast->lhs.get());
            retf = getF(ast->lhs.get());
            retr = getR(ast->rhs.get());
            break;
    }
}
void LocationVisitor::visitComplgtExpr(ASTNS::ComplgtExpr *ast)
{
    switch (ast->form)
    {
        case ASTNS::ComplgtExpr::Form::ATA:
            retl = getL(ast->lhs.get());
            retf = getF(ast->lhs.get());
            retr = getR(ast->rhs.get());
            break;
    }
}
void LocationVisitor::visitDeclList(ASTNS::DeclList *ast)
{
    switch (ast->form)
    {
        case ASTNS::DeclList::Form::AA:
            retl = getL(ast->decllist.get());
            retf = getF(ast->decllist.get());
            retr = getR(ast->decl.get());
            break;
    }
}
void LocationVisitor::visitEmptyStmt(ASTNS::EmptyStmt *ast)
{
    switch (ast->form)
    {
        case ASTNS::EmptyStmt::Form::T:
            retl = ast->semi.start;
            retf = ast->semi.sourcefile;
            retr = ast->semi.end;
            break;
    }
}
void LocationVisitor::visitExprStmt(ASTNS::ExprStmt *ast)
{
    switch (ast->form)
    {
        case ASTNS::ExprStmt::Form::AT:
            retl = getL(ast->expr.get());
            retf = getF(ast->expr.get());
            retr = ast->semi.end;
            break;
    }
}
void LocationVisitor::visitFunction(ASTNS::Function *ast)
{
    switch (ast->form)
    {
        case ASTNS::Function::Form::TATTTA:
            retl = ast->fun.start;
            retf = ast->fun.sourcefile;
            retr = ast->cparn.end;
            break;
        case ASTNS::Function::Form::TATTATA:
            retl = ast->fun.start;
            retf = ast->fun.sourcefile;
            retr = ast->cparn.end;
            break;
    }
}
void LocationVisitor::visitMultExpr(ASTNS::MultExpr *ast)
{
    switch (ast->form)
    {
        case ASTNS::MultExpr::Form::ATA:
            retl = getL(ast->lhs.get());
            retf = getF(ast->lhs.get());
            retr = getR(ast->rhs.get());
            break;
    }
}
void LocationVisitor::visitParam(ASTNS::Param *ast)
{
    switch (ast->form)
    {
        case ASTNS::Param::Form::AT:
            retl = getL(ast->type.get());
            retf = getF(ast->type.get());
            retr = ast->name.end;
            break;
    }
}
void LocationVisitor::visitParamList(ASTNS::ParamList *ast)
{
    switch (ast->form)
    {
        case ASTNS::ParamList::Form::ATA:
            retl = getL(ast->paramlist.get());
            retf = getF(ast->paramlist.get());
            retr = getR(ast->param.get());
            break;
    }
}
void LocationVisitor::visitPrimaryExpr(ASTNS::PrimaryExpr *ast)
{
    switch (ast->form)
    {
        case ASTNS::PrimaryExpr::Form::T:
            retl = ast->value.start;
            retf = ast->value.sourcefile;
            retr = ast->value.end;
            break;
        case ASTNS::PrimaryExpr::Form::TAT:
            retl = ast->oparn.start;
            retf = ast->oparn.sourcefile;
            retr = ast->cparn.end;
            break;
    }
}
void LocationVisitor::visitRetStmt(ASTNS::RetStmt *ast)
{
    switch (ast->form)
    {
        case ASTNS::RetStmt::Form::TAT:
            retl = ast->ret.start;
            retf = ast->ret.sourcefile;
            retr = ast->semi.end;
            break;
        case ASTNS::RetStmt::Form::TT:
            retl = ast->ret.start;
            retf = ast->ret.sourcefile;
            retr = ast->semi.end;
            break;
    }
}
void LocationVisitor::visitStmtList(ASTNS::StmtList *ast)
{
    switch (ast->form)
    {
        case ASTNS::StmtList::Form::AA:
            retl = getL(ast->stmtlist.get());
            retf = getF(ast->stmtlist.get());
            retr = getR(ast->stmt.get());
            break;
    }
}
void LocationVisitor::visitTernaryExpr(ASTNS::TernaryExpr *ast)
{
    switch (ast->form)
    {
        case ASTNS::TernaryExpr::Form::ATATA:
            retl = getL(ast->cond.get());
            retf = getF(ast->cond.get());
            retr = getR(ast->falses.get());
            break;
    }
}
void LocationVisitor::visitUnaryExpr(ASTNS::UnaryExpr *ast)
{
    switch (ast->form)
    {
        case ASTNS::UnaryExpr::Form::TA:
            retl = ast->op.start;
            retf = ast->op.sourcefile;
            retr = getR(ast->operand.get());
            break;
    }
}
void LocationVisitor::visitVarStmt(ASTNS::VarStmt *ast)
{
    switch (ast->form)
    {
        case ASTNS::VarStmt::Form::TAAT:
            retl = ast->var.start;
            retf = ast->var.sourcefile;
            retr = ast->semi.end;
            break;
    }
}
void LocationVisitor::visitVarStmtItem(ASTNS::VarStmtItem *ast)
{
    switch (ast->form)
    {
        case ASTNS::VarStmtItem::Form::TTA:
            retl = ast->name.start;
            retf = ast->name.sourcefile;
            retr = getR(ast->expr.get());
            break;
        case ASTNS::VarStmtItem::Form::T:
            retl = ast->name.start;
            retf = ast->name.sourcefile;
            retr = ast->name.end;
            break;
    }
}
void LocationVisitor::visitVarStmtItemList(ASTNS::VarStmtItemList *ast)
{
    switch (ast->form)
    {
        case ASTNS::VarStmtItemList::Form::ATA:
            retl = getL(ast->varstmtitemlist.get());
            retf = getF(ast->varstmtitemlist.get());
            retr = getR(ast->varstmtitem.get());
            break;
    }
}
// This code was autogenerated - see the utils/ directory

// LOCVISITOR IMPL END
// constructors for location {{{1
Location::Location(Token const &t): start(t.start), end(t.end), file(t.sourcefile) {}
Location::Location(IR::Value const &v): Location(v.ast()) {}
Location::Location(IR::Value const *v): Location(v->ast()) {}
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
        reportAbortNoh(concatMsg("Location constructor called with nullptr ast"));
    else
        reportAbortNoh(concatMsg("Location constructor reached invalid ast type: ", typeid(ast).name()));
}
// Error methods {{{1
Error::Error(MsgType type, Location const &location, std::string message): type(type), location(location), message(message) {}
Error& Error::underline(Underline const &underline)
{
    underlines.push_back(underline);
    return *this;
}
Error& Error::span(Location const &start, Location const &end)
{
    if (start.file != end.file)
        std::abort();

    spans.push_back(Span {*start.file, start.start, end.end});
    return *this;
}

// Error report method {{{1
inline std::string attr(std::string const &ansicode, std::string const &message, bool noreset=false)
{
    if (ansiCodesEnabled())
    {
        if (noreset)
            return ansicode + message;
        else
            return ansicode + message + A_RESET;
    }
    else
        return message;
}
void Error::report() const
{
    switch (type)
    {
        case Error::MsgType::ERROR:
            std::cerr << attr(A_BOLD A_FG_RED, "Error");
            break;
        case Error::MsgType::WARNING:
            std::cerr << attr(A_BOLD A_FG_MAGENTA, "Warning");
            break;
        case Error::MsgType::INTERR:
            std::cerr << "!!! - " << attr(A_BOLD A_FG_RED, "Internal error");
            break;
    }
    std::string::const_iterator const fstart = location.file->source.cbegin();
    std::cerr << " at " << attr(A_FG_CYAN, location.file->filename, true) << ":" << getLineN(fstart, location.start) << ":" << getColN(fstart, location.start) << A_RESET << ": " << message << "\n";

    using showloc = std::pair<const File*, int>; // in order to have a copy assignment constructor for sorting
    std::vector<showloc> showlocs;

    for (Span const &span : spans)
    {
        for (int i = getLineN(span.file.source.begin(), span.start); i < getLineN(span.file.source.begin(), span.end); ++i)
            showlocs.push_back(showloc(&span.file, i));
    }

    for (Error::Underline const &u : underlines)
    {
        std::string::const_iterator begin = u.location.file->source.begin();
        for (int i = getLineN(begin, u.location.start); i <= getLineN(begin, u.location.end - 1); ++i)
            showlocs.push_back(showloc(u.location.file, i));
    }

    std::sort(showlocs.begin(), showlocs.end(), [](showloc const &a, showloc const &b) {
                return a.second < b.second;
            });
    std::stable_sort(showlocs.begin(), showlocs.end(), [](showloc const &a, showloc const &b) {
                return a.first->filename < b.first->filename;
            });

    int maxlinepad = 0;
    // i + 1 < instead of i < size - 1 because - 1 can overflow to the highest value and become true
    for (size_t i = 0; i + 1 < showlocs.size(); )
    {
        if (showlocs[i].first == showlocs[i + 1].first && showlocs[i].second == showlocs[i + 1].second)
            showlocs.erase(showlocs.begin() + i + 1);
        else
            ++i;
    }

    for (size_t i = 0; i + 1 < showlocs.size(); ++i)
    {
        if (showlocs[i].first == showlocs[i + 1].first && showlocs[i + 1].second - showlocs[i].second > 1 && showlocs[i + 1].second - showlocs[i].second <= 3)
        {
            for (int j = showlocs[i].second + 1; j < showlocs[i + 1].second; ++j)
                showlocs.insert(showlocs.begin() + i + 1, showloc(showlocs[i].first, j));
        }
    }

    for (showloc const &s : showlocs)
    {
        int linew = 1, linenr = s.second;
        while (linenr /= 10)
            ++linew;
        maxlinepad = std::max(linew, maxlinepad);
    }

    std::string pad (maxlinepad + 1, ' ');
    File const *lastfile = nullptr;
    int lastnr = -1;
    for (showloc const &sl : showlocs)
    {
        if (sl.first != lastfile)
        {
            std::cerr << pad << "> " << attr(A_FG_CYAN, sl.first->filename) << std::endl;
            lastnr = -1;
        }

        if (sl.second != lastnr + 1 && lastnr != -1)
        {
            std::ios origState (nullptr);
            origState.copyfmt(std::cerr);
            std::cerr << std::setw(maxlinepad) << std::right << std::string(maxlinepad, '.') << " | ...\n";
            std::cerr.copyfmt(origState);
        }

        {
            std::ios origState (nullptr);
            origState.copyfmt(std::cerr);
            std::cerr << std::setw(maxlinepad) << std::right << sl.second;
            std::cerr.copyfmt(origState);
        }

        std::cerr << " | ";

        std::string::const_iterator lstart, lend;
        getLine(lstart, lend, *sl.first, sl.second);

        std::vector<Underline const *> lchars;
        using lunderlinety = std::pair<Underline const *, int>;
        std::vector<lunderlinety> lunderlines;

        lchars.reserve(std::distance(lstart, lend));

        auto itInLoc = [](std::string::const_iterator const &i, Location const &l)
        {
            return i >= l.start && i < l.end;
        };

        bool needsecond = false;
        for (std::string::const_iterator i = lstart; i < lend; ++i)
        {
            for (Underline const &u : underlines)
                if (itInLoc(i, u.location))
                {
                    lunderlinety pair (&u, getColN(u.location.file->source.begin(), u.location.end - 1));
                    if (u.location.end - 1 == i && u.messages.size()) // can only ever be one location where this underline ends
                        lunderlines.push_back(pair);
                    needsecond = true;
                }

            Underline const *charu = nullptr;
            for (Underline const &u : underlines)
                if (itInLoc(i, u.location))
                {
                    charu = &u;
                    break;
                }

            lchars.push_back(charu);

            if (charu && charu->messages.size())
                std::cerr << attr(A_BOLD, attr(charu->messages[0].color, std::string(1, *i)));
            else if (charu)
                std::cerr << attr(A_BOLD, std::string(1, *i));
            else
                std::cerr << *i;
        }

        std::cerr << std::endl;

        if (needsecond)
        {
            std::cerr << pad << "| ";
            for (Underline const *&i : lchars)
            {
                if (i && i->messages.size()) // in a underline
                    std::cerr << attr(A_BOLD, attr(i->messages[0].color, std::string(1, i->ch)));
                else if (i)
                    std::cerr << attr(A_BOLD, std::string(1, i->ch));
                else
                    std::cerr << " ";
            }
            std::cerr << std::endl;

            if (lunderlines.size())
            {
                std::sort(lunderlines.begin(), lunderlines.end(), [](lunderlinety const &i1, lunderlinety const &i2)
                    {
                        return i1.second > i2.second; // sort in reverse
                    });

                for (auto i = lunderlines.begin(); i < lunderlines.end(); ++i)
                {
                    size_t msgi = 0;
                    for (Error::Underline::Message const &message : i->first->messages)
                    {
                        std::cerr << pad << "| ";
                        for (auto j = lunderlines.end() - 1; j >= i; --j)
                        {
                            int last = j + 1 == lunderlines.end() ? 0 : (j + 1)->second;
                            int diff = j->second - last;
                            if (!diff)
                                continue;

                            int pamt = diff - 1;

                            std::cerr << std::string(pamt, ' ');
                            if (j != i)
                                std::cerr << '|';
                            else
                                if (msgi == i->first->messages.size() - 1)
                                    std::cerr << '`';
                                else
                                    std::cerr << '|';
                        }

                        std::cerr << attr(message.color, "-- " + message.type) << ": " << message.message << std::endl;

                        ++msgi;
                    }
                }
            }
        }

        lastfile = sl.first;
        lastnr = sl.second;
    }
}
void Error::reportAbort()
{
    report();
    std::abort();
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
    std::cerr << "!!! - " << attr(A_BOLD A_FG_RED, "Internal error") << " " << message << std::endl;
    std::abort();
}
void invalidTok(std::string const &name, Token const &underline)
{
    Error(Error::MsgType::INTERR, underline, "invalid token for " + name)
        .underline(Error::Underline(underline, '!')
            .error("invalid token")
            .note("for " + name))
        .reportAbort();
}
void calledWithOpTyNEthis(std::string const &classN, std::string const &fnn, std::string const &opname, IR::Value const *op)
{
    Error(Error::MsgType::INTERR, op, classN + "::" + fnn + " called with " + opname + " type != this")
        .underline(Error::Underline(op, '^')
            .error(opname + " type != this"))
        .reportAbort();
}
void outOSwitchDDefaultLab(std::string const &fnn, Location const &highlight)
{
    Error(Error::MsgType::INTERR, highlight, fnn + " went out of switch despite default label")
        .underline(Error::Underline(highlight, '^')
            .error("out of switch"))
        .reportAbort();
}
void fCalled(std::string const &fnn)
{
    reportAbortNoh(fnn + " called");
}
void outOSwitchNoh(std::string const &fnn)
{
    reportAbortNoh(fnn + " went out of switch");
}
