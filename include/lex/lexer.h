#pragma once

#include <string>
#include <vector>
#include <queue>

#include "utils/file.h"
#include "lex/token.h"

class Lexer {
public:
    Lexer(File &sourcefile);

    Located<Token> next_token();

private:
    void lex_more();
    void add_token(Located<Token> tok);
    std::queue<Located<Token>> token_backlog;

    template <typename T>
    Located<TokenData> make_error_token();

    bool at_end();

    char advance();
    bool match(char c);

    char peek();
    char peekpeek();
    char consumed();

    void start_to_end();

    void lex_digit(char current);
    void lex_identifier(bool apostrophes_allowed);

    Located<Token> make_token(Token t);
    template <typename Err>
    Located<Token> make_error_token();

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

    Token get_identifier_type();

    File &sourcefile;
};

