#include "lexer.h"

Lexer::Lexer(std::string &source) : start(source.begin()), end(source.begin()), line(1), column(1), srcend(source.end()){ }

Token Lexer::nextToken() {
    start = end;

    if (atEnd())
        return makeToken(TokenType::EOF_);

    char c = advance();

    switch (c) {
        case '(': return makeToken(TokenType::OPAREN); break;
        case ')': return makeToken(TokenType::CPAREN); break;
        case ',': return makeToken(TokenType::COMMA); break;
        case '.': return makeToken(TokenType::PERIOD); break;
        case '-': return makeToken(TokenType::MINUS); break;
        case '+': return makeToken(TokenType::PLUS); break;
        case ';': return makeToken(TokenType::SEMICOLON); break;
        case '/': return makeToken(TokenType::SLASH); break;
        case '*': return makeToken(TokenType::ASTERISK); break;
    }

    return makeErrorToken("Error lexing: unexpected character");
}

bool Lexer::atEnd() {
    return start + 1 == srcend;
}

char Lexer::advance() {
    ++column;

    return *(end++);
}

char Lexer::peek() {
    return *(end);
}

Token Lexer::makeErrorToken(std::string message) {
    Token token = makeToken(TokenType::ERROR);

    token.message = message;

    return token;
}

Token Lexer::makeToken(TokenType type) {
    Token token;

    token.type = type;
    token.start = start;
    token.end = end;
    token.line = line;
    token.column = column - 1;

    return token;
}
