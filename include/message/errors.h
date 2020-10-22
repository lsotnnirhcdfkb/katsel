#pragma once

#include "lex/tokentype.h"
#include "typing/type.h"
#include "value/value.h"
#include "parse/ast.h"

// Location struct {{{1
struct Location
{
    std::string::iterator start;
    std::string::iterator end;
    File const *file;

    Location(Token const &t);
    Location(std::string::iterator start, std::string::iterator end, File const *file);
    Location(Value const &v);

    Location(ASTNS::Expr *a);
    Location(ASTNS::Decl *a);
    Location(ASTNS::Type *a);
    Location(ASTNS::Stmt *a);
};
// functions {{{1
namespace msg
{
    void reportLexTok                 (Token const &t);

    void expectedPrimaryOrUnary       (Token const &t);
    void expectedType                 (Token const &t);
    void expectedDecl                 (Token const &t);

    void expectedTokGotTok            (Token const &t, TokenType got, TokenType expected);
    void reportAssertConsumeErr       (Token const &t, std::string const &message);

    void duplicateFunction            (Token const &f);
    void cannotRedefineVariable       (Token const &varname);
    void typeNoOp                     (Value const &lhs, Token const &op);
    void invalidROperand              (Value const &lop, Token const &op, Value const &rop);
    void invalidCast                  (Value const &v, Type *bty, Type *ety);
    void undefVar                     (Token const &varname);
    void cannotCall                   (Value const &callee);
    void invalidAssign                (Value const &target, Token const &eq);
    void voidVarNotAllowed            (Location const &voidTok);
    void cannotPick2Tys               (Value const &v1, Value const &v2);

    void noNullPtrLit                 (Token const &nullptrlit);
    void noStringLit                  (Token const &nullptrlit);
    void invalidTok                   (std::string const &name, Token const &primary);
    void calledWithOpTyNEthis         (std::string const &classN, std::string const &fnn, std::string const &opname, Value const &op);
    void outOSwitchDDefaultLab        (std::string const &fnn, Location const &highlight);
    void fCalled                      (std::string const &fnn);
    void outOSwitchNoh                (std::string const &fnn);
}
