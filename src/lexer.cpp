#include "lexer.h"

struct Lexer {
    size_t start;
    size_t length;
    size_t line;
};

Lexer lexer;

namespace LexerNS {
    void startLexer() {
        lexer.start = 0;
        lexer.length = 0;
        lexer.line = 1;
    }

    Token nextToken() {

    }

}

