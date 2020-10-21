#include "message/errors.h"
#include "visit/visitor.h"
#include "message/ansistuff.h"
#include <iostream>
#include <cstdlib>

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
// Reporting functions {{{1
// helpers {{{2
// apply attr to string {{{3
inline std::string attr(std::string const &ansicode, std::string const &message)
{
    if (ansiCodesEnabled())
        return ansicode + message + A_RESET;
    else
        return message;
}
// print message types {{{3
void printErr()
{
    std::cout << attr(A_BOLD A_FG_RED, "Error");
}
void printWarn()
{
    std::cout << attr(A_BOLD A_FG_MAGENTA, "Warning");
}
void printDebug()
{
    std::cout << attr(A_BOLD A_FG_CYAN, "Debug");
}
void printIntErr()
{
    std::cout << "!!! - " << attr(A_BOLD A_FG_RED, "Internal error");
}
// print message locations {{{3
void printAtFile(File const *file)
{
    std::cout << " in " << attr(A_FG_CYAN, file->filename);
}
void printAtFileLC(Location const &l)
{
    std::cout << " at " << attr(A_FG_CYAN, l.file->filename); // TODO: convert string iterators to line and column
}
// print lines and underlines {{{3
void printLine(File const *file, int line)
{

}
void printUnderline(int startc, int endc)
{

}
// print other things {{{3
void printColon()
{
    std::cout << ": ";
}
// actually reporting errors {{{2
namespace msg
{
    void unterminatedCharLit          (Token const &t);
    void unterminatedStrLit           (Token const &t);
    void invalidNumLiteralBase        (Token const &t);
    void nonDecimalFloatingPoint      (Token const &t);
    void unexpectedCharacter          (Token const &t);

    void expectedTokGotTok            (Token const &t, TokenType got, TokenType expected);
    void expectedPrimaryOrUnary       (Token const &t);
    void expectedType                 (Token const &t);
    void expectedEOFTok               (Token const &t);
    void expectedDecl                 (Token const &t);

    void duplicateFunction            (Token const &t);
    void cannotRedefineVariable       (Token const &t);
    void typeNoOp                     (Token const &t, Type *ty, Token op);
    void invalidROperand              (Value const &lop, Token &op, Type *rty);
    void invalidCast                  (Location l, Type *bty, Type *ety);
    void undefVar                     (Token const &t);
    void cannotCall                   (Token const &t);
    void invalidAssign                (Token const &target, Token const &eq);
    void voidVarNotAllowed            (Token const &t);
}
