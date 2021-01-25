#pragma once

#include <string>
#include <vector>

#include "utils/file.h"
#include "lex/token.h"
#include "lex/tokentype.h"

class Lexer {
public:
    Lexer(File &sourcefile);

    Token next_token();

private:
    bool at_end();

    char advance();
    bool match(char c);

    char peek();
    char peekpeek();
    char consumed();

    void start_to_end();

    Token lex_digit(char current);
    Token lex_identifier(bool apostrophes_allowed);

    Token make_error_token(Token::ErrFunc err_func);
    Token make_token(TokenType type);

    std::string::iterator start;
    std::string::iterator end;

    int startline;
    int startcolumn;
    int endline;
    int endcolumn;

    int indent;
    bool dedenting;
    std::vector<int> indentstack;

    std::string::iterator srcstart;
    std::string::iterator srcend;

    TokenType get_identifier_type();

    File &sourcefile;
};

