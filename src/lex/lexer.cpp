#include "lex/lexer.h"
#include "utils/assert.h"
#include "message/errmsgs.h"

// constructors {{{1
Lexer::Lexer(File &sourcefile): start(sourcefile.source.begin()), end(start), startline(1), startcolumn(1), endline(1), endcolumn(1), indent(0), srcstart(sourcefile.source.begin()), srcend(sourcefile.source.end()), sourcefile(sourcefile)
{
    indentstack.push(0);
}

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
    if (consumed() != '\n' || start == srcstart) // if not at beginning of new line
    {
        bool atWh = true;
        while (atWh)
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
                        advance(); // consume '/'
                        advance(); // consume '*'
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

    startToEnd();

    // at the beginning of a line
    if (consumed() == '\n') {
        bool blankline = true;

        while (blankline) // assume this is a blank line
        {
            indent = 0;
            while (true)
            {
                if (atEnd())
                    break;
                else if (peek() == ' ')
                {
                    ++indent;
                    advance();
                }
                else if (peek() == '\t')
                {
                    indent = (indent / 8 + 1) * 8; // go up to nearest multiple of 8
                    advance();
                }
                else
                    break;
            }

            if (atEnd())
                blankline = false;
            else if (peek() == '\n') // if after the leading whitespace of this line there is \n then this is a blank line with only whitespace
            {
                blankline = true;
                advance(); // consume \n
                startToEnd();
            }
            else // if after the leading whitespace of this line, there is not \n then this is a non-blank line
                blankline = false;
        }
    }

    if (indent > indentstack.top())
    {
        indentstack.push(indent);
        return makeToken(TokenType::INDENT);
    }
    else if (indent < indentstack.top())
    {
        indentstack.pop();
        return makeToken(TokenType::DEDENT);
    }

    startToEnd();

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
void Lexer::startToEnd()
{
    start = end;
    startline = endline;
    startcolumn = endcolumn;
}
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
