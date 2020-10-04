#include "parser.h"

#include "tokentype.h"
#include "errors.h"

// Parser constructor {{{1
Parser::Parser(Lexer &l, File &sourcefile): lexer(l), sourcefile(sourcefile)
{
    advance();
    prevToken.type = TokenType::SOF;
}
// helper methods {{{1
void Parser::advance()
{
    prevToken = currToken;
    while (true)
    {
        currToken = lexer.nextToken();

        if (currToken.type != TokenType::ERROR) break; // continue loop if it is an error token

        // override advance in error
        // it's there to prevent infinite loops, but in this function,
        // we don't need it to because it might cause problems
        reportError(currToken, currToken.message, sourcefile);
    }
}

bool Parser::checkConsume(TokenType type)
{
    if (check(type))
    {
        advance();
        return true;
    }
    return false;
}

bool Parser::check(TokenType type)
{
    return peek().type == type;
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
    while (!(check(TokenType::SEMICOLON) || check(TokenType::CCURB)) && !atEnd()) advance(); // advance until next token is semicolon

    if (check(TokenType::SEMICOLON))
        advance(); // consume semicolon

    // if doesnt advance then peek is of type eof
}
