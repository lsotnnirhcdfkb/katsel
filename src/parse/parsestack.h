#pragma once
#include <vector>
#include <memory>
#include "lex/token.h"
#include "parse/parser.h"

struct stackitem
{
    stackitem(size_t state): state(state), istok(false), isinitial(true) {}
    stackitem(size_t state, Token const &t): state(state), istok(true), isinitial(false), tok(t) {}
    stackitem(size_t state, std::unique_ptr<ASTNS::AST> ast): state(state), istok(false), isinitial(false), ast(std::move(ast)) {}
    int state;
    bool istok;
    bool isinitial;
    Token tok;
    std::unique_ptr<ASTNS::AST> ast;
};


bool errorRecovery(Parser &p, std::vector<stackitem> &stack, Token &lookahead, Error &e);
bool singleTok(Parser &p, std::vector<stackitem> &stack, Token &lookahead, Error &e);
bool panicMode(Parser &p, std::vector<stackitem> &stack, Token &lookahead, Error &e);

template <typename AST>
size_t getGoto(size_t state);
bool _parse(Parser &p, std::vector<stackitem> &stack, bool istrial, std::unique_ptr<ASTNS::DeclB> &out, Token const &_lookahead);
