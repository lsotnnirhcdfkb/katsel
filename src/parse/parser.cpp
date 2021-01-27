#include "parse/parser.h"
#include "parserlocal.h"
#include "lex/lexer.h"

Parser::Parser(Lexer &l, File &sourcefile): lexer(l), sourcefile(sourcefile), errored(false) {}

std::unique_ptr<ASTNS::CUB> Parser::parse() {
    std::unique_ptr<ASTNS::CUB> ret (nullptr);

    std::vector<stackitem> stack;
    stack.emplace_back(0);

    _parse(*this, stack, false, ret, consume());

    if (errored)
        return nullptr;

    return ret;
}

Token Parser::consume() {
    bool lastboom = false;
    while (true) {
        Token cur (lexer.next_token());
        if (cur.is<Tokens::Error>()) {
            errored = true;
            (*cur.as<Tokens::Error>().errf)(cur);
        } else if (cur.is<Tokens::Boom>()) {
            lastboom = true;
        } else if (lastboom && cur.is<Tokens::Newline>()) {
            lastboom = false;
        } else {
            return cur;
        }
    }
}
