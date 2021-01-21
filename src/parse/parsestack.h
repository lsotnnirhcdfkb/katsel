#pragma once
#include <vector>
#include <memory>
#include <variant>
#include "lex/token.h"
#include "parse/parser.h"

// nonterminal enum {{{
enum class NonTerminal {
    // NONTERM ENUM START
// The following code was autogenerated - see the utils/ directory
    augment,
    CU,
    AnotherParam,
    Param,
    ParamSegment,
    ParamList,
    AnotherArg,
    Arg,
    ArgSegment,
    ArgList,
    AnotherVarStmtItem,
    VarStmtItem,
    VarStmtItemSegment,
    VarStmtItemList,
    AnotherStmt,
    Stmt,
    StmtList,
    AnotherDecl,
    Decl,
    DeclList,
    AnotherImplMember,
    ImplMember,
    ImplMemberList,
    ParamList_OPT,
    ArgList_OPT,
    StmtList_OPT,
    ImplMemberList_OPT,
    Expr_OPT,
    Expr,
    VarStmt_OPT,
    VarStmt,
    LineEnding_OPT,
    LineEnding,
    TypeAnnotation_OPT,
    TypeAnnotation,
    FunctionDecl,
    ImplDecl,
    Block,
    Type,
    ImplBody,
    ExprStmt,
    RetStmt,
    NotBlockedExpr,
    BlockedExpr,
    BracedBlock,
    IndentedBlock,
    PathType,
    PointerType,
    ThisType,
    Path,
    NormalParam,
    ThisParam,
    AssignmentExpr,
    IfExpr,
    WhileExpr,
    BinOrExpr,
    BinAndExpr,
    CompEQExpr,
    CompLGTExpr,
    BitXorExpr,
    BitOrExpr,
    BitAndExpr,
    BitShiftExpr,
    AdditionExpr,
    MultExpr,
    UnaryExpr,
    CastExpr,
    CallExpr,
    FieldAccessExpr,
    MethodCallExpr,
    PrimaryExpr,
    PathExpr,
// This code was autogenerated - see the utils/ directory
    // NONTERM ENUM END
};
// }}}

struct tokenitem { Token tok; };
struct astitem { std::unique_ptr<ASTNS::AST> ast; NonTerminal nt; };
struct initialitem { int dummy; };

struct stackitem {
    int state;
    std::variant<tokenitem, astitem, initialitem> item;

    stackitem(int state): state(state), item(initialitem {0}) {}
    stackitem(int state, Token const &t): state(state), item(tokenitem {t}) {}
    stackitem(int state, std::unique_ptr<ASTNS::AST> ast, NonTerminal nt): state(state), item(astitem {std::move(ast), nt}) {}
};

struct errorstate {
    Parser &p;
    std::vector<stackitem> &stack;
    Token const &lasttok;
    Token &lookahead;
    Token const olh;

    inline errorstate(Parser &p, std::vector<stackitem> &stack, Token &lasttok, Token &lookahead) : p(p), stack(stack), lasttok(lasttok), lookahead(lookahead), olh(lookahead) {}
};

bool errorRecovery(errorstate const &e, std::vector<std::string> const &expectations);
bool singleTok(errorstate const &e, std::vector<std::string> const &expectations);
bool panicMode(errorstate const &e, std::vector<std::string> const &expectations);

template <typename AST>
size_t getGoto(size_t state);
bool _parse(Parser &p, std::vector<stackitem> &stack, bool istrial, std::unique_ptr<ASTNS::CUB> &out, Token const &_lookahead);
