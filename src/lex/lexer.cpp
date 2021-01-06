#include "lex/lexer.h"
#include "utils/assert.h"
#include "message/errmsgs.h"

// constructors {{{1
Lexer::Lexer(File &sourcefile): start(sourcefile.source.begin()), end(start), startline(1), startcolumn(1), endline(1), endcolumn(1), indent(0), dedenting(false), srcstart(sourcefile.source.begin()), srcend(sourcefile.source.end()), sourcefile(sourcefile) {
    indentstack.push_back(0);
}

// resetToTok {{{1
void Lexer::resetToTok(Token const &t) {
    start = end = t.start;
    startline = endline = t.line;
    startcolumn = endcolumn = t.column;

    ASSERT(t.sourcefile == &sourcefile)
}
// lex digit and lex identifier {{{1
static bool isAlpha(char c) {
    return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c == '_';
}
Token Lexer::lexDigit(char current) {
    enum class IntBase {
        dec,
        oct,
        hex,
        bin,
        inv
    };

    auto isDigit = [](char const &c, IntBase const &base) {
        switch (base) {
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

    if (current != '0' || isDigit(peek(), IntBase::dec) || !isAlpha(peek())) {
        base = IntBase::dec;
        ++ndigits;
    } else
        switch (advance()) {
            case 'o': base = IntBase::oct; break;
            case 'x': base = IntBase::hex; break;
            case 'b': base = IntBase::bin; break;
            default:
                      base = IntBase::inv;
        }

    char next;
    while (!atEnd() && (isDigit(next = peek(), IntBase::hex) || next == '.')) {
        advance();

        if (next == '.')
            isfloat = true;
        else {
            if (base != IntBase::inv && !isDigit(next, base))
                intvalid = false;
            ++ndigits;
        }
    }

    if (isfloat) {
        if (base != IntBase::dec) return makeErrorToken(ERR_NONDECIMAL_FLOATLIT);
        if (!intvalid) return makeErrorToken(ERR_INVALID_CHAR_FLOATLIT);
        return makeToken(TokenType::FLOATLIT);
    } else
        if (base == IntBase::inv)
            return makeErrorToken(ERR_INVALID_INTLIT_BASE);
        else if (ndigits == 0)
            return makeErrorToken(ERR_INTLIT_NO_DIGITS);
        else if (!intvalid)
            return makeErrorToken(ERR_INVALID_CHAR_FOR_BASE);
        else {
            switch (base) {
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
Token Lexer::lexIdentifier() {
    while (isAlpha(peek()) || (peek() >= '0' && peek() <= '9')) advance();

    TokenType idenType = getIdentifierType();
    return makeToken(idenType);
}
// nextToken {{{1
Token Lexer::nextToken() {
    {
        bool atWh = true;
        bool findingindent = end == srcstart || consumed() == '\n';
        if (findingindent) {
            startToEnd();
            indent = 0;
        }

        while (atWh) {
            switch (peek()) {
                case '\r':
                    advance(); break;
                case ' ':
                    if (findingindent) {
                        ++indent;
                    }
                    advance();
                    break;
                case '\t':
                    if (findingindent) {
                        indent = (indent / 8 + 1) * 8; // go up to nearest multiple of 8
                    }
                    advance();
                    break;

                case '\\':
                    if (peekpeek() == '\n') {
                        advance();
                        advance();
                        ++endline;
                        endcolumn = 1;
                    } else {
                        advance();
                        return makeErrorToken(ERR_CHAR_AFTER_BACKSLASH);
                    }
                    break;

                case '/':
                    if (peekpeek() == '/')
                        while (peek() != '\n' && !atEnd()) advance();
                    else if (peekpeek() == '*') {
                        advance(); // consume '/'
                        advance(); // consume '*'
                        while (!(peek() == '*' && peekpeek() == '/') && !atEnd()) {
                            if (peek() == '\n') {
                                ++endline;
                                endcolumn = 1;
                            }
                            advance();
                        }

                        if (atEnd())
                            return makeErrorToken(ERR_UNTERM_MULTILINE_COMMENT);

                        advance(); // advance twice to consume the * and /
                        advance();
                    } else
                        atWh = false;
                    break;

                case '\n':
                    if (findingindent) {
                        // the only way you can get to a \n while finding an indent is if the entire line is blank, because you would have started at the beginning of a line
                        advance();
                        ++endline;
                        endcolumn = 1;
                        indent = 0;
                    } else
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

    if (indent > indentstack.back()) {
        if (dedenting) {
            dedenting = false;
            return makeErrorToken(ERR_DEDENT_NOMATCH);
        }

        startToEnd();
        indentstack.push_back(indent);
        return makeToken(TokenType::INDENT);
    } else if (indent < indentstack.back()) {
        dedenting = true;
        indentstack.pop_back();
        return makeToken(TokenType::DEDENT);
    } else if (dedenting) // indent == indentstack.top()
        dedenting = false;

    startToEnd();

    if (atEnd())
        return makeToken(TokenType::EOF_);

    char current = advance();

    switch (current) {
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
        case '&': return makeToken(match('&') ? TokenType::DOUBLEAMPER : (match('=') ? TokenType::AMPEREQUAL : TokenType::AMPER));
        case '|': return makeToken(match('|') ? TokenType::DOUBLEPIPE  : (match('=') ? TokenType::PIPEEQUAL  : TokenType::PIPE));

                  // arrows
        case '-': return makeToken(match('-') ? TokenType::DOUBLEMINUS : (match('=') ? TokenType::MINUSEQUAL : (match('>') ? TokenType::RIGHTARROW : TokenType::MINUS)));
        case '<': return makeToken(match('<') ? (match('=') ? TokenType::DOUBLELESSEQUAL : TokenType::DOUBLELESS) : (match('=') ? TokenType::LESSEQUAL : (match('-') ? TokenType::LEFTARROW : TokenType::LESS)));

                  // double, doubleequal, singleequal, single
                  //                  if matches double ? (is double so check if it has equal after it                          ) : (is not double so check if it has equal after it          )
        case '>': return makeToken(match('>') ? (match('=') ? TokenType::DOUBLEGREATEREQUAL : TokenType::DOUBLEGREATER) : (match('=') ? TokenType::GREATEREQUAL : TokenType::GREATER));

        case '\'':
        case '"':
            char startingQuote = consumed();
            while (peek() != startingQuote && !atEnd() && peek() != '\n')
                advance();

            if (startingQuote == '"' && peek() != '"')
                return makeErrorToken(ERR_UNTERM_STRLIT);
            else if (startingQuote == '\'' && peek() != '\'')
                return makeErrorToken(ERR_UNTERM_CHARLIT);

            advance(); // consume closing quote

            if (startingQuote == '\'') {
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
TokenType Lexer::getIdentifierType() {
    switch (*(start + 0)) {
        case 'c':
            switch (*(start + 1)) {
                case 'l':
                    if (std::distance(start, end) == 5 && *(start + 2) == 'a' && *(start + 3) == 's' && *(start + 4) == 's') return TokenType::CLASS;
                    break;
                case 'o':
                    if (std::distance(start, end) == 8 && *(start + 2) == 'n' && *(start + 3) == 't' && *(start + 4) == 'i' && *(start + 5) == 'n' && *(start + 6) == 'u' && *(start + 7) == 'e') return TokenType::CONTINUE;
                    break;
            }
            break;
        case 'd':
            if (std::distance(start, end) == 4 && *(start + 1) == 'a' && *(start + 2) == 't' && *(start + 3) == 'a') return TokenType::DATA;
            break;
        case 'i':
            switch (*(start + 1)) {
                case 'm':
                    if (std::distance(start, end) == 4 && *(start + 2) == 'p' && *(start + 3) == 'l') return TokenType::IMPL;
                    break;
                case 'f':
                    if (start + 2 == end) return TokenType::IF;
                    break;
            }
            break;
        case 'f':
            switch (*(start + 1)) {
                case 'u':
                    if (std::distance(start, end) == 3 && *(start + 2) == 'n') return TokenType::FUN;
                    break;
                case 'o':
                    if (std::distance(start, end) == 3 && *(start + 2) == 'r') return TokenType::FOR;
                    break;
                case 'a':
                    if (std::distance(start, end) == 5 && *(start + 2) == 'l' && *(start + 3) == 's' && *(start + 4) == 'e') return TokenType::FALSELIT;
                    break;
            }
            break;
        case 'v':
            if (std::distance(start, end) == 3 && *(start + 1) == 'a' && *(start + 2) == 'r') return TokenType::VAR;
            break;
        case 'm':
            switch (*(start + 1)) {
                case 'u':
                    if (std::distance(start, end) == 3 && *(start + 2) == 't') return TokenType::MUT;
                    break;
                case 'a':
                    if (std::distance(start, end) == 5 && *(start + 2) == 't' && *(start + 3) == 'c' && *(start + 4) == 'h') return TokenType::MATCH;
                    break;
            }
            break;
        case 'l':
            if (std::distance(start, end) == 3 && *(start + 1) == 'e' && *(start + 2) == 't') return TokenType::LET;
            break;
        case 'r':
            if (std::distance(start, end) == 6 && *(start + 1) == 'e' && *(start + 2) == 't' && *(start + 3) == 'u' && *(start + 4) == 'r' && *(start + 5) == 'n') return TokenType::RETURN;
            break;
        case 'w':
            if (std::distance(start, end) == 5 && *(start + 1) == 'h' && *(start + 2) == 'i' && *(start + 3) == 'l' && *(start + 4) == 'e') return TokenType::WHILE;
            break;
        case 'e':
            if (std::distance(start, end) == 4 && *(start + 1) == 'l' && *(start + 2) == 's' && *(start + 3) == 'e') return TokenType::ELSE;
            break;
        case 'b':
            switch (*(start + 1)) {
                case 'r':
                    if (std::distance(start, end) == 5 && *(start + 2) == 'e' && *(start + 3) == 'a' && *(start + 4) == 'k') return TokenType::BREAK;
                    break;
                case 'o':
                    if (std::distance(start, end) == 4 && *(start + 2) == 'o' && *(start + 3) == 'm') return TokenType::BOOM;
                    break;
            }
            break;
        case 't':
            if (std::distance(start, end) == 4 && *(start + 1) == 'r' && *(start + 2) == 'u' && *(start + 3) == 'e') return TokenType::TRUELIT;
            break;
        case 'n':
            if (std::distance(start, end) == 7 && *(start + 1) == 'u' && *(start + 2) == 'l' && *(start + 3) == 'l' && *(start + 4) == 'p' && *(start + 5) == 't' && *(start + 6) == 'r') return TokenType::NULLPTRLIT;
            break;
        case 'a':
            if (std::distance(start, end) == 6 && *(start + 1) == 's' && *(start + 2) == 's' && *(start + 3) == 'e' && *(start + 4) == 'r' && *(start + 5) == 't') return TokenType::ASSERT;
            break;
    }

    return TokenType::IDENTIFIER;
}
// This code was autogenerated - see the utils/ directory
// KWGEN END
// helpers {{{1
void Lexer::startToEnd() {
    start = end;
    startline = endline;
    startcolumn = endcolumn;
}
bool Lexer::atEnd() {
    return end >= srcend;
}
bool Lexer::match(char c) {
    if (atEnd())
        return false;

    if (peek() == c) {
        advance();
        return true;
    }

    return false;
}
char Lexer::advance() {
    ++endcolumn;
    return *(end++);
}
char Lexer::peek() {
    return *end;
}
char Lexer::peekpeek() {
    return *(end + 1);
}
char Lexer::consumed() {
    return *(end - 1);
}

// making tokens {{{1
Token Lexer::makeToken(TokenType type) {
    if (type == TokenType::DEDENT)
        return Token {type, start, start, nullptr, startline, startcolumn - 1, &sourcefile};
    else
        return Token {type, start, end, nullptr, startline, startcolumn - 1, &sourcefile};
}
Token Lexer::makeErrorToken(void (*errf)(Token const &)) {
    Token token = makeToken(TokenType::ERROR);
    token.errf = errf;
    return token;
}
