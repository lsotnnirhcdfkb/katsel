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
    _49, _0, _50, _22, _52, _51, _53, _21, _55, _54, _56, _10, _58, _57, _59, _6, _60, _61, _1, _62, _63, _5, _64, _65, _66, _67, _68, _69, _25, _70, _7, _71, _11, _72, _12, _73, _16, _2, _3, _13, _17, _4, _8, _9, _27, _26, _14, _15, _20, _18, _19, _48, _24, _23, _30, _28, _29, _31, _32, _33, _34, _35, _36, _37, _38, _39, _40, _42, _41, _43, _44, _45, _46, _47, 
    // NONTERM ENUM END
};
// }}}

template <typename T>
struct ASTItem { T ast; NonTerminal nt; };
struct TokenItem { Located<TokenData> tok; };
struct InitialItem { int dummy; };

struct StackItem {
    int state;
    std::variant<TokenItem, InitialItem,
        // PARSESTACK ITEM TYPES START
        ASTItem<std::unique_ptr<ASTNS::AST>>,
        ASTItem<std::unique_ptr<ASTNS::Arg>>,
        ASTItem<std::unique_ptr<ASTNS::ArgList>>,
        ASTItem<std::unique_ptr<ASTNS::Block>>,
        ASTItem<std::unique_ptr<ASTNS::CU>>,
        ASTItem<std::unique_ptr<ASTNS::Decl>>,
        ASTItem<std::unique_ptr<ASTNS::DeclList>>,
        ASTItem<std::unique_ptr<ASTNS::Expr>>,
        ASTItem<std::unique_ptr<ASTNS::ExprStmt>>,
        ASTItem<std::unique_ptr<ASTNS::FunctionDecl>>,
        ASTItem<std::unique_ptr<ASTNS::IfExpr>>,
        ASTItem<std::unique_ptr<ASTNS::ImplMember>>,
        ASTItem<std::unique_ptr<ASTNS::ImplMemberList>>,
        ASTItem<std::unique_ptr<ASTNS::Param>>,
        ASTItem<std::unique_ptr<ASTNS::ParamB>>,
        ASTItem<std::unique_ptr<ASTNS::ParamList>>,
        ASTItem<std::unique_ptr<ASTNS::Path>>,
        ASTItem<std::unique_ptr<ASTNS::PathType>>,
        ASTItem<std::unique_ptr<ASTNS::PointerType>>,
        ASTItem<std::unique_ptr<ASTNS::PureLocation>>,
        ASTItem<std::unique_ptr<ASTNS::RetStmt>>,
        ASTItem<std::unique_ptr<ASTNS::Stmt>>,
        ASTItem<std::unique_ptr<ASTNS::StmtList>>,
        ASTItem<std::unique_ptr<ASTNS::ThisParam>>,
        ASTItem<std::unique_ptr<ASTNS::ThisType>>,
        ASTItem<std::unique_ptr<ASTNS::Type>>,
        ASTItem<std::unique_ptr<ASTNS::VarStmt>>,
        ASTItem<std::unique_ptr<ASTNS::VarStmtItem>>,
        ASTItem<std::unique_ptr<ASTNS::VarStmtItemList>>,
        ASTItem<std::unique_ptr<ASTNS::WhileExpr>>
        // PARSESTACK ITEM TYPES END
        > item;

    template <typename T>
    StackItem(int state, T &&thing): state(state), item(std::forward<T>(thing)) {}

    StackItem(int state): state(state), item(InitialItem {}) {}
};

template <typename AST>
size_t get_goto(size_t state);
std::unique_ptr<ASTNS::CUB> _parse(Parser &p);
