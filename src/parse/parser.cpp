#include "parse/parser.h"
#include "parsestack.h"

#include "lex/tokentype.h"
#include "lex/lexer.h"

#include <sstream>

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
        Token cur (lexer.nextToken());
        if (cur.type == TokenType::ERROR) {
            errored = true;
            cur.errf.match([&cur] (Token::ErrFunc const &e) {
                    (*e)(cur);
                },
                [] {
                    reportAbortNoh("Error token without error function");
                });
        } else if (cur.type == TokenType::BOOM) {
            lastboom = true;
        } else if (lastboom && cur.type == TokenType::NEWLINE) {
            lastboom = false;
        } else {
            return cur;
        }
    }
}
