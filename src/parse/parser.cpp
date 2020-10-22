#include "parse/parser.h"

#include "lex/tokentype.h"
#include "message/errors.h"

#include <sstream>

// Parser constructor {{{1
Parser::Parser(Lexer &l, File &sourcefile): lexer(l), sourcefile(sourcefile), ispanic(false)
{
    consume();
    prevToken = l.makeSOF();
}
// helper methods {{{1
Token& Parser::consume()
{
    prevToken = currToken;
    while (true)
    {
        currToken = lexer.nextToken();

        if (currToken.type != TokenType::ERROR) break; // continue loop if it is an error token

        msg::reportLexTok(prev());
    }

    return prev();
}

bool Parser::assertConsume(TokenType type, std::string const &message)
{
    bool correct = check(type);

    if (!correct)
    {
        if (message.size() == 0)
            msg::expectedTokGotTok(peek(), peek().type, type);
        else
            msg::reportAssertConsumeErr(peek(), message);
    }

    consume();
    return correct;
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
    while (!check(TokenType::FUN) && !atEnd()) consume(); // consume until next token is fun
}
