#include "parse/parser.h"
#include "parserlocal.h"
#include "lex/lexer.h"

Parser::Parser(Lexer &l, File &sourcefile): lexer(l), sourcefile(sourcefile), errored(false) {}

std::unique_ptr<ASTNS::CUB> Parser::parse() {
    return _parse(*this);
}

Located<TokenData> Parser::consume() {
    bool lastboom = false;
    while (true) {
        Located<TokenData> cur (lexer.next_token());
        if (Tokens::is<Tokens::Error>(cur.value)) {
            errored = true;
            (*Tokens::as<Tokens::Error>(cur.value).errf)(cur.span);
        } else if (Tokens::is<Tokens::Boom>(cur.value)) {
            lastboom = true;
        } else if (lastboom && Tokens::is<Tokens::Newline>(cur.value)) {
            lastboom = false;
        } else {
            return cur;
        }
    }
}
