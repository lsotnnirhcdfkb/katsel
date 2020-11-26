
#include "lex/lexer.h"
#include "message/errmsgs.h"

Lexer::Lexer(File &sourcefile) : start(sourcefile.source.begin()), end(sourcefile.source.begin()), line(1), column(1), nextline(1), nextcolumn(1), srcend(sourcefile.source.end()), sourcefile(sourcefile) {}
Lexer::Lexer(Token const &t) : start(t.start), end(t.start), line(t.line), column(t.column), nextline(t.line), nextcolumn(t.column), srcend(t.sourcefile->source.end()), sourcefile(*t.sourcefile) {}

// {{{ getIdentifierType
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
                    switch (*(start + 2))
                    {
                        case 'i':
                            if (std::distance(start, end) == 4 && std::string(start + 3, end) == "d") return TokenType::VOID;
                            break;
                        case 'l':
                            if (std::distance(start, end) == 8 && std::string(start + 3, end) == "atile") return TokenType::VOLATILE;
                            break;
                    }
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
                case 'a':
                    if (std::distance(start, end) == 4 && std::string(start + 2, end) == "se") return TokenType::CASE;
                    break;
                case 'o':
                    switch (*(start + 2))
                    {
                        case 'n':
                            switch (*(start + 3))
                            {
                                case 't':
                                    if (std::distance(start, end) == 8 && std::string(start + 4, end) == "inue") return TokenType::CONTINUE;
                                    break;
                                case 's':
                                    if (std::distance(start, end) == 5 && std::string(start + 4, end) == "t") return TokenType::CONST;
                                    break;
                            }
                            break;
                    }
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
                case 'w':
                    if (std::distance(start, end) == 6 && std::string(start + 2, end) == "itch") return TokenType::SWITCH;
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
        case 't':
            switch (*(start + 1))
            {
                case 'h':
                    if (std::distance(start, end) == 4 && std::string(start + 2, end) == "is") return TokenType::THIS;
                    break;
                case 'r':
                    if (std::distance(start, end) == 4 && std::string(start + 2, end) == "ue") return TokenType::TRUELIT;
                    break;
            }
            break;
        case 'w':
            if (std::distance(start, end) == 5 && std::string(start + 1, end) == "hile") return TokenType::WHILE;
            break;
        case 'i':
            switch (*(start + 1))
            {
                case 'f':
                    if (start + 2 == end) return TokenType::IF;
                    break;
                case 'n':
                    if (std::distance(start, end) == 6 && std::string(start + 2, end) == "line") return TokenType::INLINE;
                    break;
            }
            break;
        case 'a':
            if (std::distance(start, end) == 6 && std::string(start + 1, end) == "ssert") return TokenType::ASSERT;
            break;
    }

    return TokenType::IDENTIFIER;
}
// This code was autogenerated - see the utils/ directory

// KWGEN END
// }}}
// {{{ helper functions
bool isDigit(char c)
{
    return c >= '0' && c <= '9';
}

bool isDigit(char c, TokenType base)
{ // overloaded method for dealing with non-decimal integer literals
    switch (base)
    {
        case TokenType::DECINTLIT:
            return isDigit(c);

        case TokenType::OCTINTLIT:
            return c >= '0' && c < '8';

        case TokenType::BININTLIT:
            return c == '0' || c == '1';

        case TokenType::HEXINTLIT:
            return isDigit(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F');

        default: // invalid base
            return false;
    }
}

bool isAlpha(char c)
{
    return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c == '_';
}
// }}}
// {{{ nextToken
Token Lexer::nextToken()
{
    start = end;

    if (atEnd()) // don't parse tokens
        return makeToken(TokenType::EOF_);

    {
        bool atWhitespace = true;
        while (atWhitespace)
        {
            char c = peek();
            switch (c)
            {
                case '\r':
                case ' ':
                case '\t':
                    advance();
                    break;

                case '\n':
                    nextLine();
                    advance();
                    break;

                case '/':
                    if (peekpeek() == '/')
                    { // check if this is a comment
                        while (peek() != '\n' && !atEnd()) advance();
                    }
                    else if (peekpeek() == '*')
                    { // multiline comment
                        // while next two characters are not * and /
                        while ((peek() != '*' || peekpeek() != '/') && !atEnd())
                        {
                            if (peek() == '\n') nextLine();
                            advance();
                        }

                        advance(); // advance twice to consume the * and /
                        advance();
                    }
                    else
                    {
                        atWhitespace = false;
                    }
                    break;

                default:
                    atWhitespace = false;
                    break;
            }

            if (atEnd())
            {
                atWhitespace = false;
            }
        }
    }

    start = end; // put this before so if file ends with whitespace then the whitespace is not included in the EOF token
    line = nextline;
    column = nextcolumn;

    if (atEnd()) // if file ends with whitespace
        return makeToken(TokenType::EOF_);

    char c = advance();

    switch (c)
    {
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
        case '*': return makeToken(match('=') ? TokenType::STAREQUAL : TokenType::STAR);
        case '/': return makeToken(match('=') ? TokenType::SLASHEQUAL : TokenType::SLASH);
        case '!': return makeToken(match('=') ? TokenType::BANGEQUAL : TokenType::BANG);
        case '%': return makeToken(match('=') ? TokenType::PERCENTEQUAL : TokenType::PERCENT);
        case '^': return makeToken(match('=') ? TokenType::CARETEQUAL : TokenType::CARET);

                  // double and equal and single
                  //        if matches double ? return double         : is not double so check if it has equals after it
        case '+': return makeToken(match('+') ? TokenType::DOUBLEPLUS : (match('=') ? TokenType::PLUSEQUAL : TokenType::PLUS));
        case '-': return makeToken(match('-') ? TokenType::DOUBLEMINUS : (match('=') ? TokenType::MINUSEQUAL : TokenType::MINUS));
        case '&': return makeToken(match('&') ? TokenType::DOUBLEAMPER : (match('=') ? TokenType::AMPEREQUAL : TokenType::AMPER));
        case '|': return makeToken(match('|') ? TokenType::DOUBLEPIPE : (match('=') ? TokenType::PIPEEQUAL : TokenType::PIPE));

                  // double, doubleequal, singleequal, single
                  //        if matches double ? (is double so check if it has equal after it                          ) : (is not double so check if it has equal after it          )
        case '>': return makeToken(match('>') ? (match('=') ? TokenType::DOUBLEGREATEREQUAL : TokenType::DOUBLEGREATER) : (match('=') ? TokenType::GREATEREQUAL : TokenType::GREATER));
                  //        if matches double ? (is double so check if it has equal after it                    ) : (is not double so check if it has equal after it    )
        case '<': return makeToken(match('<') ? (match('=') ? TokenType::DOUBLELESSEQUAL : TokenType::DOUBLELESS) : (match('=') ? TokenType::LESSEQUAL : TokenType::LESS));

        case 'c': // check for char literal
                  if (match('\'') || match('"'))
                  { // should consume quote
                      char startingQuote = consumed();
                      advance(); // consume character

                      if (!match(startingQuote)) return makeErrorToken(ERR_UNTERM_CHARLIT);

                      return makeToken(TokenType::CHARLIT);
                  }
                  break;

        case '"':
        case '\'':
                  // c is the starting string thing
                  while (peek() != c && !atEnd() && peek() != '\n')
                  {
                      advance();
                  }

                  if (peek() != c) return makeErrorToken(ERR_UNTERM_STRLIT);
                  advance(); // consume closing quote/apostrophe
                  return makeToken(TokenType::STRINGLIT);
    }

    if (isDigit(c))
    {
        // check for number literal
        TokenType inttype;
        bool validint = true;

        if (isDigit(peek()) || c != '0' || !isAlpha(peek()))
        {
            inttype = TokenType::DECINTLIT;
        }
        else
        {
            switch (peek())
            {
                case 'o': inttype = TokenType::OCTINTLIT; break;
                case 'b': inttype = TokenType::BININTLIT; break;
                case 'x': inttype = TokenType::HEXINTLIT; break;

                default:
                    validint = false;
            }

            advance(); // consume o, b, or x
        }

        while (isDigit(peek(), inttype) && !atEnd()) advance();

        if (peek() == '.' && isDigit(peekpeek(), inttype) && !atEnd())
        {
            // is actually a decimal and is not integer literal
            advance(); // consume decimal point

            while (isDigit(peek(), inttype) && !atEnd()) advance();

            if (!validint)
                return makeErrorToken(ERR_INVALID_INTLIT_BASE);

            if (inttype != TokenType::DECINTLIT) return makeErrorToken(ERR_NONDECIMAL_FLOATLIT);

            return makeToken(TokenType::FLOATLIT);
        }

        if (!validint)
            return makeErrorToken(ERR_INVALID_INTLIT_BASE);
        else
            return makeToken(inttype);
    }
    else if (isAlpha(c))
    {
        while (isAlpha(peek()) || isDigit(peek())) advance();

        TokenType idenType = getIdentifierType();
        return makeToken(idenType);
    }

    return makeErrorToken(ERR_UNEXECPTED_CHAR);
}
// }}}
// {{{1 other helpers
Token Lexer::makeErrorToken(void (*errf)())
{
    Token token = makeToken(TokenType::ERROR);

    token.errf = errf;

    return token;
}
