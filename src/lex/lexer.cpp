#include "lex/lexer.h"
#include "utils/assert.h"
#include "message/errmsgs.h"

// constructors {{{1
Lexer::Lexer(File &sourcefile): start(sourcefile.source.begin()), end(start), startline(1), startcolumn(1), endnextline(1), endnextcolumn(1), srcend(sourcefile.source.end()), atLineStart(true), sourcefile(sourcefile) {}
Lexer::Lexer(Token const &t): start(t.start), end(t.start), startline(t.line), startcolumn(t.column), endnextline(t.line), endnextcolumn(t.column), srcend(t.sourcefile->source.end()), atLineStart(t.column == 1), sourcefile(*t.sourcefile) {}

// resetToTok {{{1
void Lexer::resetToTok(Token const &t)
{
    start = end = t.start;
    startline = endline = t.line;
    startcolumn = endcolumn = t.column;

    ASSERT(t.sourcefile == &sourcefile)
}

// nextToken {{{1
// helpers {{{1
bool Lexer::atEnd()
{
    return end >= srcend;
}
bool Lexer::match(char c)
{
    if (atEnd())
        return false;

    if (peek() == c)
    {
        advance();
        return true;
    }

    return false;
}
char Lexer::advance()
{
    ++endcolumn;
    return *(end++);
}
char Lexer::peek()
{
    return *end;
}
char Lexer::peekpeek()
{
    return *(end + 1);
}
char Lexer::consumed()
{
    return *(end - 1);
}
// making tokens {{{1
Token Lexer::makeToken(TokenType type)
{
    return Token {type, start, end, nullptr, startline, startcolumn - 1, &sourcefile};
}
Token Lexer::makeErrorToken(void (*errf)(Token const &))
{
    Token token = makeToken(TokenType::ERROR);
    token.errf = errf;
    return token;
}
