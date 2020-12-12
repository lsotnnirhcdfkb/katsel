#include "lex/lexer.h"
#include "utils/assert.h"
#include "message/errmsgs.h"

// constructors {{{1
Lexer::Lexer(File &sourcefile): start(sourcefile.source.begin()), end(start), startline(1), startcolumn(1), endline(1), endcolumn(1), indent(0), dedenting(false), srcstart(sourcefile.source.begin()), srcend(sourcefile.source.end()), sourcefile(sourcefile)
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
// lex digit and lex identifier {{{1
static bool isAlpha(char c)
{
    return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c == '_';
}
Token Lexer::lexDigit(char current)
{
    enum class IntBase
    {
        dec,
        oct,
        hex,
        bin,
        inv
    };

    auto isDigit = [](char const &c, IntBase const &base)
        {
            switch (base)
            {
                case IntBase::dec:
                    return c >= '0' && c <= '9';

                case IntBase::oct:
                    return c >= '0' && c < '8';

                case IntBase::bin:
                    return c == '0' || c == '1';

                case IntBase::hex:
                    return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F');

                default:
                    return false;
            }
        };

    IntBase base;
    bool intvalid = true;
    bool isfloat = false;
    int ndigits = 0;

    if (current != '0' || isDigit(peek(), IntBase::dec) || !isAlpha(peek()))
    {
        base = IntBase::dec;
        ++ndigits;
    }
    else
        switch (advance())
        {
            case 'o': base = IntBase::oct; break;
            case 'x': base = IntBase::hex; break;
            case 'b': base = IntBase::bin; break;
            default:
                      base = IntBase::inv;
        }

    char next;
    while (!atEnd() && (isDigit(next = peek(), IntBase::hex) || next == '.'))
    {
        advance();

        if (next == '.')
            isfloat = true;
        else
        {
            if (base != IntBase::inv && !isDigit(next, base))
                intvalid = false;
            ++ndigits;
        }
    }

    if (isfloat)
    {
        if (base != IntBase::dec) return makeErrorToken(ERR_NONDECIMAL_FLOATLIT);
        if (!intvalid) return makeErrorToken(ERR_INVALID_CHAR_FLOATLIT);
        return makeToken(TokenType::FLOATLIT);
    }
    else
        if (base == IntBase::inv)
            return makeErrorToken(ERR_INVALID_INTLIT_BASE);
        else if (ndigits == 0)
            return makeErrorToken(ERR_INTLIT_NO_DIGITS);
        else if (!intvalid)
            return makeErrorToken(ERR_INVALID_CHAR_FOR_BASE);
        else
        {
            switch (base)
            {
                case IntBase::dec:
                    return makeToken(TokenType::DECINTLIT);
                case IntBase::hex:
                    return makeToken(TokenType::HEXINTLIT);
                case IntBase::oct:
                    return makeToken(TokenType::OCTINTLIT);
                case IntBase::bin:
                    return makeToken(TokenType::BININTLIT);
                default:
                    return makeToken(TokenType::ERROR);
            }
        }
}
Token Lexer::lexIdentifier()
{
    while (isAlpha(peek()) || (peek() >= '0' && peek() <= '9')) advance();

    TokenType idenType = getIdentifierType();
    return makeToken(idenType);
}
// nextToken {{{1
Token Lexer::nextToken()
{
    {
        bool atWh = true;
        bool findingindent = start == srcstart || consumed() == '\n';
        if (findingindent)
            indent = 0;

        while (atWh)
        {
            switch (peek())
            {
                case '\r': advance(); break;
                case ' ':
                    if (findingindent)
                        ++indent;
                    advance();
                    break;
                case '\t':
                    if (findingindent)
                        indent = (indent / 8 + 1) * 8; // go up to nearest multiple of 8
                    advance();
                    break;

                case '\\':
                    if (peekpeek() == '\n')
                    {
                        advance();
                        advance();
                        ++endline;
                        endcolumn = 1;
                    }
                    break;

                case '/':
                    if (peekpeek() == '/')
                        while (peek() != '\n' && !atEnd()) advance();
                    else if (peekpeek() == '*')
                    {
                        advance(); // consume '/'
                        advance(); // consume '*'
                        while (!(peek() == '*' && peekpeek() == '/') && !atEnd())
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

                case '\n':
                    if (findingindent) // the only way you can get to a \n while finding an indent is if the entire line is blank, because you would have started at the beginning of a line
                    {
                        advance();
                        ++endline;
                        endcolumn = 1;
                        indent = 0;
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

    if (indent > indentstack.top())
    {
        if (dedenting)
        {
            dedenting = false;
            return makeErrorToken(ERR_DEDENT_NOMATCH);
        }

        indentstack.push(indent);
        return makeToken(TokenType::INDENT);
    }
    else if (indent < indentstack.top())
    {
        dedenting = true;
        indentstack.pop();
        return makeToken(TokenType::DEDENT);
    }
    else if (dedenting) // indent == indentstack.top()
        dedenting = false;

    startToEnd();

    if (atEnd())
        return makeToken(TokenType::EOF_);

    char current = advance();

    switch (current)
    {
        case '\n':
            ++endline;
            endcolumn = 1;
            return makeToken(TokenType::NEWLINE);

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

        case '\'':
        case '"':
            char startingQuote = consumed();
            while (peek() != startingQuote && !atEnd() && peek() != '\n')
            {
                advance();
            }

            if (startingQuote == '"' && peek() != '"') return makeErrorToken(ERR_UNTERM_STRLIT);
            else if (startingQuote == '\'' && peek() != '\'') return makeErrorToken(ERR_UNTERM_CHARLIT);

            advance(); // consume closing quote

            if (startingQuote == '\'')
            {
                if (std::distance(start, end) != 3) return makeErrorToken(ERR_MULTICHAR_CHARLIT);
                else return makeToken(TokenType::CHARLIT);
            }

            return makeToken(TokenType::STRINGLIT);
    }

    if (current >= '0' && current <= '9')
        return lexDigit(current);
    else if (isAlpha(current))
        return lexIdentifier();

    return makeErrorToken(ERR_UNEXPECTED_CHAR);
}
// getIdentifierType {{{1
// KWGEN START

// The following code was autogenerated - see the utils/ directory
/// Check if an idenetifier token is a keyword type and return that type, or just return TokenType::IDENTIFIER
TokenType Lexer::getIdentifierType()
{
    switch (*(start + 0))
    {
        case 'v':
            switch (*(start + 1))
            {
                case 'o':
                    if (std::distance(start, end) == 4 && std::string(start + 2, end) == "id") return TokenType::VOID;
                    break;
                case 'a':
                    if (std::distance(start, end) == 3 && std::string(start + 2, end) == "r") return TokenType::VAR;
                    break;
            }
            break;
        case 'f':
            switch (*(start + 1))
            {
                case 'l':
                    if (std::distance(start, end) == 5 && std::string(start + 2, end) == "oat") return TokenType::FLOAT;
                    break;
                case 'u':
                    if (std::distance(start, end) == 3 && std::string(start + 2, end) == "n") return TokenType::FUN;
                    break;
                case 'o':
                    if (std::distance(start, end) == 3 && std::string(start + 2, end) == "r") return TokenType::FOR;
                    break;
                case 'a':
                    if (std::distance(start, end) == 5 && std::string(start + 2, end) == "lse") return TokenType::FALSELIT;
                    break;
            }
            break;
        case 'b':
            switch (*(start + 1))
            {
                case 'o':
                    if (std::distance(start, end) == 4 && std::string(start + 2, end) == "ol") return TokenType::BOOL;
                    break;
                case 'r':
                    switch (*(start + 2))
                    {
                        case 'e':
                            switch (*(start + 3))
                            {
                                case 'a':
                                    switch (*(start + 4))
                                    {
                                        case 'k':
                                                if (start + 5 == end) return TokenType::BREAK;
                                            switch (*(start + 5))
                                            {
                                                case 'a':
                                                    if (std::distance(start, end) == 8 && std::string(start + 6, end) == "ll") return TokenType::BREAKALL;
                                                    break;
                                                case 't':
                                                    if (std::distance(start, end) == 7 && std::string(start + 6, end) == "o") return TokenType::BREAKTO;
                                                    break;
                                            }
                                            break;
                                    }
                                    break;
                            }
                            break;
                    }
                    break;
            }
            break;
        case 'd':
            switch (*(start + 1))
            {
                case 'o':
                    if (std::distance(start, end) == 6 && std::string(start + 2, end) == "uble") return TokenType::DOUBLE;
                    break;
                case 'e':
                    if (std::distance(start, end) == 7 && std::string(start + 2, end) == "fault") return TokenType::DEFAULT;
                    break;
            }
            break;
        case 'c':
            switch (*(start + 1))
            {
                case 'h':
                    if (std::distance(start, end) == 4 && std::string(start + 2, end) == "ar") return TokenType::CHAR;
                    break;
                case 'l':
                    if (std::distance(start, end) == 5 && std::string(start + 2, end) == "ass") return TokenType::CLASS;
                    break;
                case 'o':
                    if (std::distance(start, end) == 8 && std::string(start + 2, end) == "ntinue") return TokenType::CONTINUE;
                    break;
            }
            break;
        case 'u':
            switch (*(start + 1))
            {
                case 'i':
                    switch (*(start + 2))
                    {
                        case 'n':
                            switch (*(start + 3))
                            {
                                case 't':
                                    switch (*(start + 4))
                                    {
                                        case '8':
                                            if (start + 5 == end) return TokenType::UINT8;
                                            break;
                                        case '1':
                                            if (std::distance(start, end) == 6 && std::string(start + 5, end) == "6") return TokenType::UINT16;
                                            break;
                                        case '3':
                                            if (std::distance(start, end) == 6 && std::string(start + 5, end) == "2") return TokenType::UINT32;
                                            break;
                                        case '6':
                                            if (std::distance(start, end) == 6 && std::string(start + 5, end) == "4") return TokenType::UINT64;
                                            break;
                                    }
                                    break;
                            }
                            break;
                    }
                    break;
            }
            break;
        case 's':
            switch (*(start + 1))
            {
                case 'i':
                    switch (*(start + 2))
                    {
                        case 'n':
                            switch (*(start + 3))
                            {
                                case 't':
                                    switch (*(start + 4))
                                    {
                                        case '8':
                                            if (start + 5 == end) return TokenType::SINT8;
                                            break;
                                        case '1':
                                            if (std::distance(start, end) == 6 && std::string(start + 5, end) == "6") return TokenType::SINT16;
                                            break;
                                        case '3':
                                            if (std::distance(start, end) == 6 && std::string(start + 5, end) == "2") return TokenType::SINT32;
                                            break;
                                        case '6':
                                            if (std::distance(start, end) == 6 && std::string(start + 5, end) == "4") return TokenType::SINT64;
                                            break;
                                    }
                                    break;
                            }
                            break;
                    }
                    break;
            }
            break;
        case 'n':
            switch (*(start + 1))
            {
                case 'a':
                    if (std::distance(start, end) == 9 && std::string(start + 2, end) == "mespace") return TokenType::NAMESPACE;
                    break;
                case 'u':
                    if (std::distance(start, end) == 7 && std::string(start + 2, end) == "llptr") return TokenType::NULLPTRLIT;
                    break;
            }
            break;
        case 'e':
            switch (*(start + 1))
            {
                case 'n':
                    if (std::distance(start, end) == 4 && std::string(start + 2, end) == "um") return TokenType::ENUM;
                    break;
                case 'l':
                    if (std::distance(start, end) == 4 && std::string(start + 2, end) == "se") return TokenType::ELSE;
                    break;
            }
            break;
        case 'r':
            if (std::distance(start, end) == 6 && std::string(start + 1, end) == "eturn") return TokenType::RETURN;
            break;
        case 'w':
            if (std::distance(start, end) == 5 && std::string(start + 1, end) == "hile") return TokenType::WHILE;
            break;
        case 'i':
            if (std::distance(start, end) == 2 && std::string(start + 1, end) == "f") return TokenType::IF;
            break;
        case 'p':
            if (std::distance(start, end) == 7 && std::string(start + 1, end) == "attern") return TokenType::PATTERN;
            break;
        case 't':
            if (std::distance(start, end) == 4 && std::string(start + 1, end) == "rue") return TokenType::TRUELIT;
            break;
        case 'a':
            if (std::distance(start, end) == 6 && std::string(start + 1, end) == "ssert") return TokenType::ASSERT;
            break;
    }

    return TokenType::IDENTIFIER;
}
// This code was autogenerated - see the utils/ directory

// KWGEN END
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
