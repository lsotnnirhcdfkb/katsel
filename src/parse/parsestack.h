#pragma once
#include <vector>
#include <memory>
#include "lex/token.h"
#include "parse/parser.h"

struct stackitem
{
    int state;
    stackitem(size_t state): state(state) {}
    virtual void dummy() {}
};

struct tokstackitem : public stackitem
{
    Token tok;
    tokstackitem(size_t state, Token tok): stackitem(state), tok(tok) {}
};

struct aststackitem : public stackitem
{
    std::unique_ptr<ASTNS::AST> ast;
    aststackitem(size_t state, std::unique_ptr<ASTNS::AST> ast): stackitem(state), ast(std::move(ast)) {}
};

bool errorRecovery(Parser &p, std::vector<std::unique_ptr<stackitem>> &stack, Token &lookahead, Error &e);
bool singleTok(Parser &p, std::vector<std::unique_ptr<stackitem>> &stack, Token &lookahead, Error &e);
bool panicMode(Parser &p, std::vector<std::unique_ptr<stackitem>> &stack, Token &lookahead, Error &e);

template <typename AST>
size_t getGoto(size_t state);
bool _parse(Parser &p, bool istrial, std::unique_ptr<ASTNS::Decls> &out);
