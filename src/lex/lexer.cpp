#include "lex/lexer.h"
#include "utils/assert.h"
#include "message/errmsgs.h"

// constructors {{{1
Lexer::Lexer(File &sourcefile): start(sourcefile.source.begin()), end(start), startline(1), startcolumn(1), endline(1), endcolumn(1), srcend(sourcefile.source.end()), sourcefile(sourcefile) {}
Lexer::Lexer(Token const &t): start(t.start), end(t.start), startline(t.line), startcolumn(t.column), endline(t.line), endcolumn(t.column), srcend(t.sourcefile->source.end()), sourcefile(*t.sourcefile) {}

// resetToTok {{{1
void Lexer::resetToTok(Token const &t)
{
    start = end = t.start;
    startline = endline = t.line;
    startcolumn = endcolumn = t.column;

    ASSERT(t.sourcefile == &sourcefile)
}
// nextToken {{{1
Token Lexer::nextToken()
{
    if (consumed() != '\n') // if not at beginning of new line
    {
        bool atWh = true;
        while (atWh == false)
        {
            switch (peek())
            {
                case '\r':
                case ' ':
                case '\t':
                    advance();
                    break;

                case '/':
                    if (peekpeek() == '/')
                        while (peek() != '\n' && !atEnd()) advance();
                    else if (peekpeek() == '*')
                    {
                        while (!(peek() == '*' || peekpeek() == '/') && !atEnd())
                        {
                            if (peek() == '\n')
                            {
                                ++endline;
                                endcolumn = 1;
                            }
                            advance();
                        }

                        if (atEnd())
                            return makeErrorToken(ERR_UNTERM_MULTILINE_COMMENT);

                        advance(); // advance twice to consume the * and /
                        advance();
                    }
                    else
                        atWh = false;
                    break;

                default:
                    atWh = false;
                    break;
            }

            if (atEnd())
                atWh = false;
        }
    }

    start = end;
    startline = endline;
    startcolumn = endcolumn;

    if (atEnd())
        return makeToken(TokenType::EOF_);

    char current = advance();

    switch (current)
    {
        case '\n': return makeToken(TokenType::NEWLINE);

        case '(': return makeToken(TokenType::OPARN);
        case ')': return makeToken(TokenType::CPARN);
        case '[': return makeToken(TokenType::OSQUB);
        case ']': return makeToken(TokenType::CSQUB);
        case '{': return makeToken(TokenType::OCURB);
        case '}': return makeToken(TokenType::CCURB);
        case ',': return makeToken(TokenType::COMMA);
        case '.': return makeToken(TokenType::PERIOD);
        case ';': return makeToken(TokenType::SEMICOLON);
        case '?': return makeToken(TokenType::QUESTION);
        case '~': return makeToken(TokenType::TILDE);

        // double and single
        case '=': return makeToken(match('=') ? TokenType::DOUBLEEQUAL : TokenType::EQUAL);
        case ':': return makeToken(match(':') ? TokenType::DOUBLECOLON : TokenType::COLON);

        // equal and single
        case '*': return makeToken(match('=') ? TokenType::STAREQUAL    : TokenType::STAR);
        case '/': return makeToken(match('=') ? TokenType::SLASHEQUAL   : TokenType::SLASH);
        case '!': return makeToken(match('=') ? TokenType::BANGEQUAL    : TokenType::BANG);
        case '%': return makeToken(match('=') ? TokenType::PERCENTEQUAL : TokenType::PERCENT);
        case '^': return makeToken(match('=') ? TokenType::CARETEQUAL   : TokenType::CARET);

        // double and equal and single
        case '+': return makeToken(match('+') ? TokenType::DOUBLEPLUS  : (match('=') ? TokenType::PLUSEQUAL  : TokenType::PLUS));
        case '-': return makeToken(match('-') ? TokenType::DOUBLEMINUS : (match('=') ? TokenType::MINUSEQUAL : TokenType::MINUS));
        case '&': return makeToken(match('&') ? TokenType::DOUBLEAMPER : (match('=') ? TokenType::AMPEREQUAL : TokenType::AMPER));
        case '|': return makeToken(match('|') ? TokenType::DOUBLEPIPE  : (match('=') ? TokenType::PIPEEQUAL  : TokenType::PIPE));

        // double, doubleequal, singleequal, single
        //                  if matches double ? (is double so check if it has equal after it                          ) : (is not double so check if it has equal after it          )
        case '>': return makeToken(match('>') ? (match('=') ? TokenType::DOUBLEGREATEREQUAL : TokenType::DOUBLEGREATER) : (match('=') ? TokenType::GREATEREQUAL : TokenType::GREATER));
        case '<': return makeToken(match('<') ? (match('=') ? TokenType::DOUBLELESSEQUAL    : TokenType::DOUBLELESS   ) : (match('=') ? TokenType::LESSEQUAL    : TokenType::LESS   ));
    }

    return makeErrorToken(ERR_UNEXPECTED_CHAR);
}
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
