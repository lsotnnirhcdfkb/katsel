#include "parse/parser.h"

#include "lex/tokentype.h"
#include "message/errors.h"

#include <sstream>

// Parser constructor {{{1
Parser::Parser(Lexer &l, File &sourcefile): lexer(l), sourcefile(sourcefile)
{
    consume();
    prevToken.type = TokenType::SOF;
}
// helper methods {{{1
Token& Parser::consume()
{
    prevToken = currToken;
    while (true)
    {
        currToken = lexer.nextToken();

        if (currToken.type != TokenType::ERROR) break; // continue loop if it is an error token

        reportError(currToken, currToken.message, sourcefile);
    }

    return prev();
}

bool Parser::checkConsume(TokenType type)
{
    if (check(type))
    {
        consume();
        return true;
    }
    return false;
}

bool Parser::check(TokenType type)
{
    return peek().type == type;
}

Token& Parser::assertConsume(TokenType type, std::string message)
{
    std::stringstream ss;

    if (message.size() == 0)
    {
        ss << "Unexpected token " << peek().type << ", expected " << type << std::endl;
        message = ss.str();
    }

    bool correct = check(type);
    consume();

    if (!correct)
        reportError(prev(), message, sourcefile);

    return prev();
}

bool Parser::atEnd()
{
    return check(TokenType::EOF_);
}

Token& Parser::peek()
{
    return currToken;
}

Token& Parser::prev()
{
    return prevToken;
}

// panic methods {{{1
void Parser::panic()
{
    ispanic = true;
}

void Parser::unpanic()
{
    ispanic = false;
}

void Parser::syncTokens()
{
    while (!(check(TokenType::SEMICOLON) || check(TokenType::CCURB)) && !atEnd()) consume(); // consume until next token is semicolon

    if (check(TokenType::SEMICOLON))
        consume(); // consume semicolon

    // if doesnt consume then peek is of type eof
}
