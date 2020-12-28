#pragma once
#include <vector>
#include <memory>
#include <variant>
#include "lex/token.h"
#include "parse/parser.h"

// nonterminal enum {{{
enum class NonTerminal
{
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
    ParamList_OPT,
    ArgList_OPT,
    StmtList_OPT,
    ImplRet_OPT,
    ImplRet,
    Expr_OPT,
    Expr,
    VarStmt_OPT,
    VarStmt,
    LineEnding_OPT,
    LineEnding,
    FunctionDecl,
    Type,
    Block,
    ExprStmt,
    RetStmt,
    NotBlockedExpr,
    BlockedExpr,
    BracedBlock,
    IndentedBlock,
    PrimitiveType,
    PointerType,
    AssignmentExpr,
    IfExpr,
    ForExpr,
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
    PrimaryExpr,
// This code was autogenerated - see the utils/ directory
    // NONTERM ENUM END
};
// }}}
struct tokenitem { Token tok; };
struct astitem { std::unique_ptr<ASTNS::AST> ast; NonTerminal nt; };
struct initialitem {};

struct stackitem
{
    int state;
    std::variant<tokenitem, astitem, initialitem> item;

    stackitem(int state): state(state), item(initialitem()) {}
    stackitem(int state, Token const &t): state(state), item(tokenitem {t}) {}
    stackitem(int state, std::unique_ptr<ASTNS::AST> ast, NonTerminal nt): state(state), item(astitem {std::move(ast), nt}) {}
};

struct errorstate
{
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
