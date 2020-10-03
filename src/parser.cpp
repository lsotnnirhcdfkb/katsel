#include "parser.h"

#include "tokentype.h"
#include "errors.h"

Parser::Parser(Lexer &l, File &sourcefile): lexer(l), sourcefile(sourcefile)
{
    advance();
    prevToken.type = TokenType::SOF;
}

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

Token& Parser::peek()
{
    return currToken;
}

Token& Parser::prev()
{
    return prevToken;
}
