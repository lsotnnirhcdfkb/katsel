#pragma once
#include <vector>
#include <memory>
#include <variant>
#include "lex/token.h"
#include "ast/ast.h"
#include "parse/parser.h"

// nonterminal enum {{{
enum class NonTerminal {
    // NONTERM ENUM START
    _48, _0, _49, _21, _51, _50, _52, _20, _54, _53, _55, _10, _57, _56, _58, _6, _59, _60, _1, _61, _62, _5, _63, _64, _65, _66, _67, _68, _24, _69, _7, _70, _11, _71, _15, _2, _3, _12, _16, _4, _8, _9, _26, _25, _13, _14, _19, _17, _18, _47, _23, _22, _29, _27, _28, _30, _31, _32, _33, _34, _35, _36, _37, _38, _39, _41, _40, _42, _43, _44, _45, _46, 
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

bool error_recovery(errorstate const &e, std::vector<std::string> const &expectations);
bool single_tok(errorstate const &e, std::vector<std::string> const &expectations);
bool panic_mode(errorstate const &e, std::vector<std::string> const &expectations);

template <typename AST>
size_t get_goto(size_t state);
bool _parse(Parser &p, std::vector<stackitem> &stack, bool istrial, std::unique_ptr<ASTNS::CUB> &out, Token const &_lookahead);
