#pragma once

#include <string>
#include <vector>
#include <queue>

#include "utils/file.h"
#include "lex/token.h"

class Lexer {
public:
    Lexer(File &sourcefile);

    Located<TokenData> next_token();

private:
    void lex_more();
    void add_token(Located<TokenData> const &tok);
    std::queue<Located<TokenData>> token_backlog;

    bool at_end();

    char advance();
    bool match(char c);

    char peek();
    char peekpeek();
    char consumed();

    void start_to_end();

    void lex_digit(char current);
    void lex_identifier(bool apostrophes_allowed);

    Located<TokenData> make_token(TokenData const &t);

    std::string::const_iterator start;
    std::string::const_iterator end;

    int startline;
    int startcolumn;
    int endline;
    int endcolumn;

    struct IndentFrame {
        bool indentation_sensitive;
        int level;
    };
    std::vector<IndentFrame> indent_stack;

    std::string::const_iterator srcstart;
    std::string::const_iterator srcend;

    TokenData get_identifier_type();

    File &sourcefile;
};

