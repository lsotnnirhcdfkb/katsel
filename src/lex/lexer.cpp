#include "lex/lexer.h"
#include "utils/assert.h"
#include "message/errmsgs.h"

// constructors {{{1
Lexer::Lexer(File &sourcefile): start(sourcefile.source.begin()), end(start), startline(1), startcolumn(1), endline(1), endcolumn(1), indent(0), dedenting(false), srcstart(sourcefile.source.begin()), srcend(sourcefile.source.end()), sourcefile(sourcefile) {
    indentstack.push_back(0);
}
// lex digit and lex identifier {{{1
static bool is_alpha(char c) {
    return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c == '_';
}
Token Lexer::lex_digit(char current) {
    enum class IntBase {
        dec,
        oct,
        hex,
        bin,
        inv
    };

    auto is_digit = [](char const &c, IntBase const &base) {
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

    if (current != '0' || is_digit(peek(), IntBase::dec) || !is_alpha(peek())) {
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
    while (!at_end() && (is_digit(next = peek(), IntBase::hex) || next == '.')) {
        advance();

        if (next == '.')
            isfloat = true;
        else {
            if (base != IntBase::inv && !is_digit(next, base))
                intvalid = false;
            ++ndigits;
        }
    }

    if (isfloat) {
        if (base != IntBase::dec) return make_error_token(ERR_NONDECIMAL_FLOATLIT);
        if (!intvalid) return make_error_token(ERR_INVALID_CHAR_FLOATLIT);
        return make_token(TokenType::FLOATLIT);
    } else
        if (base == IntBase::inv)
            return make_error_token(ERR_INVALID_INTLIT_BASE);
        else if (ndigits == 0)
            return make_error_token(ERR_INTLIT_NO_DIGITS);
        else if (!intvalid)
            return make_error_token(ERR_INVALID_CHAR_FOR_BASE);
        else {
            switch (base) {
                case IntBase::dec:
                    return make_token(TokenType::DECINTLIT);
                case IntBase::hex:
                    return make_token(TokenType::HEXINTLIT);
                case IntBase::oct:
                    return make_token(TokenType::OCTINTLIT);
                case IntBase::bin:
                    return make_token(TokenType::BININTLIT);
                default:
                    return make_token(TokenType::ERROR);
            }
        }
}
Token Lexer::lex_identifier(bool apostrophes_allowed) {
    while (is_alpha(peek()) || (peek() >= '0' && peek() <= '9')) advance();

    if (apostrophes_allowed)
        while (peek() == '\'') advance();

    TokenType iden_type = get_identifier_type();
    return make_token(iden_type);
}
// next_token {{{1
Token Lexer::next_token() {
    {
        bool at_wh = true;
        bool findingindent = end == srcstart || consumed() == '\n';
        if (findingindent) {
            start_to_end();
            indent = 0;
        }

        while (at_wh) {
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
                        return make_error_token(ERR_CHAR_AFTER_BACKSLASH);
                    }
                    break;

                case '/':
                    if (peekpeek() == '/')
                        while (peek() != '\n' && !at_end()) advance();
                    else if (peekpeek() == '*') {
                        advance(); // consume '/'
                        advance(); // consume '*'
                        while (!(peek() == '*' && peekpeek() == '/') && !at_end()) {
                            if (peek() == '\n') {
                                ++endline;
                                endcolumn = 1;
                            }
                            advance();
                        }

                        if (at_end())
                            return make_error_token(ERR_UNTERM_MULTILINE_COMMENT);

                        advance(); // advance twice to consume the * and /
                        advance();
                    } else
                        at_wh = false;
                    break;

                case '\n':
                    if (findingindent) {
                        // the only way you can get to a \n while finding an indent is if the entire line is blank, because you would have started at the beginning of a line
                        advance();
                        ++endline;
                        endcolumn = 1;
                        indent = 0;
                    } else
                        at_wh = false;
                    break;

                default:
                    at_wh = false;
                    break;
            }

            if (at_end())
                at_wh = false;
        }
    }

    if (indent > indentstack.back()) {
        if (dedenting) {
            dedenting = false;
            return make_error_token(ERR_DEDENT_NOMATCH);
        }

        start_to_end();
        indentstack.push_back(indent);
        return make_token(TokenType::INDENT);
    } else if (indent < indentstack.back()) {
        dedenting = true;
        indentstack.pop_back();
        return make_token(TokenType::DEDENT);
    } else if (dedenting) // indent == indentstack.top()
        dedenting = false;

    start_to_end();

    if (at_end())
        return make_token(TokenType::EOF_);

    char current = advance();

    switch (current) {
        case '\n':
            ++endline;
            endcolumn = 1;
            return make_token(TokenType::NEWLINE);

        case '(': return make_token(TokenType::OPARN);
        case ')': return make_token(TokenType::CPARN);
        case '[': return make_token(TokenType::OSQUB);
        case ']': return make_token(TokenType::CSQUB);
        case '{': return make_token(TokenType::OCURB);
        case '}': return make_token(TokenType::CCURB);
        case ',': return make_token(TokenType::COMMA);
        case '.': return make_token(TokenType::PERIOD);
        case ';': return make_token(TokenType::SEMICOLON);
        case '?': return make_token(TokenType::QUESTION);
        case '~': return make_token(TokenType::TILDE);

        case '#': return make_token(TokenType::HASH);
        case '$': return make_token(TokenType::DOLLAR);

                  // double and single
        case '=': return make_token(match('=') ? TokenType::DOUBLEEQUAL : TokenType::EQUAL);
        case ':': return make_token(match(':') ? TokenType::DOUBLECOLON : TokenType::COLON);

                  // equal and single
        case '*': return make_token(match('=') ? TokenType::STAREQUAL    : TokenType::STAR);
        case '/': return make_token(match('=') ? TokenType::SLASHEQUAL   : TokenType::SLASH);
        case '!': return make_token(match('=') ? TokenType::BANGEQUAL    : TokenType::BANG);
        case '%': return make_token(match('=') ? TokenType::PERCENTEQUAL : TokenType::PERCENT);
        case '^': return make_token(match('=') ? TokenType::CARETEQUAL   : TokenType::CARET);

                  // double and equal and single
        case '+': return make_token(match('+') ? TokenType::DOUBLEPLUS  : (match('=') ? TokenType::PLUSEQUAL  : TokenType::PLUS));
        case '&': return make_token(match('&') ? TokenType::DOUBLEAMPER : (match('=') ? TokenType::AMPEREQUAL : TokenType::AMPER));
        case '|': return make_token(match('|') ? TokenType::DOUBLEPIPE  : (match('=') ? TokenType::PIPEEQUAL  : TokenType::PIPE));

                  // arrows
        case '-': return make_token(match('-') ? TokenType::DOUBLEMINUS : (match('=') ? TokenType::MINUSEQUAL : (match('>') ? TokenType::RIGHTARROW : TokenType::MINUS)));
        case '<': return make_token(match('<') ? (match('=') ? TokenType::DOUBLELESSEQUAL : TokenType::DOUBLELESS) : (match('=') ? TokenType::LESSEQUAL : (match('-') ? TokenType::LEFTARROW : TokenType::LESS)));

                  // double, doubleequal, singleequal, single
                  //                  if matches double ? (is double so check if it has equal after it                          ) : (is not double so check if it has equal after it          )
        case '>': return make_token(match('>') ? (match('=') ? TokenType::DOUBLEGREATEREQUAL : TokenType::DOUBLEGREATER) : (match('=') ? TokenType::GREATEREQUAL : TokenType::GREATER));

        case '\'':
        case '"':
            char starting_quote = consumed();
            while (peek() != starting_quote && !at_end() && peek() != '\n')
                advance();

            if (starting_quote == '"' && peek() != '"')
                return make_error_token(ERR_UNTERM_STRLIT);
            else if (starting_quote == '\'' && peek() != '\'')
                return make_error_token(ERR_UNTERM_CHARLIT);

            advance(); // consume closing quote

            if (starting_quote == '\'') {
                if (std::distance(start, end) != 3) return make_error_token(ERR_MULTICHAR_CHARLIT);
                else return make_token(TokenType::CHARLIT);
            }

            return make_token(TokenType::STRINGLIT);
    }

    if (current >= '0' && current <= '9')
        return lex_digit(current);
    else if (is_alpha(current))
        return lex_identifier(true);

    return make_error_token(ERR_UNEXPECTED_CHAR);
}
// KWMATCH START {{{1
/// Check if an idenetifier token is a keyword type and return that type, or just return TokenType::IDENTIFIER
TokenType Lexer::get_identifier_type() {
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
        case 't':
            switch (*(start + 1)) {
                case 'h':
                    if (std::distance(start, end) == 4 && *(start + 2) == 'i' && *(start + 3) == 's') return TokenType::THIS;
                    break;
                case 'r':
                    if (std::distance(start, end) == 4 && *(start + 2) == 'u' && *(start + 3) == 'e') return TokenType::TRUELIT;
                    break;
            }
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
        case 'n':
            if (std::distance(start, end) == 7 && *(start + 1) == 'u' && *(start + 2) == 'l' && *(start + 3) == 'l' && *(start + 4) == 'p' && *(start + 5) == 't' && *(start + 6) == 'r') return TokenType::NULLPTRLIT;
            break;
        case 'a':
            if (std::distance(start, end) == 6 && *(start + 1) == 's' && *(start + 2) == 's' && *(start + 3) == 'e' && *(start + 4) == 'r' && *(start + 5) == 't') return TokenType::ASSERT;
            break;
    }

    return TokenType::IDENTIFIER;
}
// KWMATCH END
// helpers {{{1
void Lexer::start_to_end() {
    start = end;
    startline = endline;
    startcolumn = endcolumn;
}
bool Lexer::at_end() {
    return end >= srcend;
}
bool Lexer::match(char c) {
    if (at_end())
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
Token Lexer::make_token(TokenType type) {
    if (type == TokenType::DEDENT)
        return Token {type, start, start, Maybe<Token::ErrFuncField>(), startline, startcolumn - 1, sourcefile};
    else
        return Token {type, start, end, Maybe<Token::ErrFuncField>(), startline, startcolumn - 1, sourcefile};
}
Token Lexer::make_error_token(Token::ErrFunc errf) {
    Token token = make_token(TokenType::ERROR);
    token.errf = Maybe<Token::ErrFuncField>(errf);
    return token;
}
