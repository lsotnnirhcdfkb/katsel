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

template <typename AST>
size_t get_goto(size_t state);

std::unique_ptr<ASTNS::CUB> _parse(Parser &p);
