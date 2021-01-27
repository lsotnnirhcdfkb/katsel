#pragma once

#include <string>
#include <vector>

#include "utils/file.h"
#include "lex/token.h"

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

    Token make_token(TokenData const &data);

    std::string::const_iterator start;
    std::string::const_iterator end;

    int startline;
    int startcolumn;
    int endline;
    int endcolumn;

    int indent;
    bool dedenting;
    std::vector<int> indentstack;

    std::string::const_iterator srcstart;
    std::string::const_iterator srcend;

    TokenData get_identifier_type();

    File &sourcefile;
};

