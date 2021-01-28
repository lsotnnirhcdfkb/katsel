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
Located<TokenData> Lexer::lex_digit(char current) {
    using Base = Tokens::IntLit::Base; // type less

    auto is_digit = [](char const &c, Tokens::IntLit::Base base) {
        switch (base) {
            case Base::DECIMAL:
                return c >= '0' && c <= '9';

            case Base::OCTAL:
                return c >= '0' && c < '8';

            case Base::BINARY:
                return c == '0' || c == '1';

            case Base::HEX:
                return (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F');
        }
    };

    Base base;

    bool base_valid = true;
    bool digits_valid = true;

    bool is_float = false;
    int num_digits = 0;
    int num_float_digits = 0;

    if (current == '0' && !is_digit(peek(), Base::DECIMAL) && is_alpha(peek())) {
        switch (advance()) {
            case 'o':
                base = Base::OCTAL; break;
            case 'x':
                base = Base::HEX; break;
            case 'b':
                base = Base::BINARY; break;
            default:
                base = Base::DECIMAL;
                base_valid = false;
        }
    } else {
        base = Base::DECIMAL;
        ++num_digits;
    }

    char next;
    while (!at_end() && (is_digit(next = peek(), Base::HEX) || next == '.')) {
        advance();

        if (next == '.')
            is_float = true;
        else {
            if (!is_digit(next, base))
                digits_valid = false;

            if (is_float)
                ++num_float_digits;
            else
                ++num_digits;
        }
    }

    // TODO: if num_float_digits == 0, rewind, this is like when its '2.', that should be Intlit, DOT, not ERR_FLOAT_LIT_NO_DIGITS
    // TODO: any radix for number literals like smalltalk

    if (!base_valid)
        return make_token(Tokens::Error { ERR_INVALID_NUMLIT_BASE });
    else if (!digits_valid)
        return make_token(Tokens::Error { ERR_INVALID_CHAR_FOR_BASE });
    else if (num_digits == 0)
        return make_token(Tokens::Error { ERR_INTLIT_NO_DIGITS });
    else
        if (is_float) {
            if (base != Base::DECIMAL) return make_token(Tokens::Error { ERR_NONDECIMAL_FLOATLIT });
            double val = stold(std::string(start, end));
            return make_token(Tokens::FloatLit { val });
        } else {
            uint64_t val = stoll(std::string(start, end));
            return make_token(Tokens::IntLit { val, base });
        }
}
Located<TokenData> Lexer::lex_identifier(bool apostrophes_allowed) {
    while (is_alpha(peek()) || (peek() >= '0' && peek() <= '9')) advance();

    if (apostrophes_allowed)
        while (peek() == '\'') advance();

    TokenData data = get_identifier_type();
    return make_token(data);
}
// next_token {{{1
Located<TokenData> Lexer::next_token() {
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
                        return make_token(Tokens::Error { ERR_CHAR_AFTER_BACKSLASH });
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
                            return make_token(Tokens::Error { ERR_UNTERM_MULTILINE_COMMENT });

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
            return make_token(Tokens::Error { ERR_DEDENT_NOMATCH });
        }

        start_to_end();
        indentstack.push_back(indent);
        return make_token(Tokens::Indent {});
    } else if (indent < indentstack.back()) {
        dedenting = true;
        indentstack.pop_back();
        return make_token(Tokens::Dedent {});
    } else if (dedenting) // indent == indentstack.top()
        dedenting = false;

    start_to_end();

    if (at_end())
        return make_token(Tokens::_EOF {});

    char current = advance();

    switch (current) {
        case '\n':
            ++endline;
            endcolumn = 1;
            return make_token(Tokens::Newline {});

        case '(': return make_token(Tokens::OParen {});
        case ')': return make_token(Tokens::CParen {});
        case '[': return make_token(Tokens::OBrack {});
        case ']': return make_token(Tokens::CBrack {});
        case '{': return make_token(Tokens::OBrace {});
        case '}': return make_token(Tokens::CBrace {});
        case ',': return make_token(Tokens::Comma {});
        case '.': return make_token(Tokens::Period {});
        case ';': return make_token(Tokens::Semicolon {});
        case '?': return make_token(Tokens::Question {});
        case '~': return make_token(Tokens::Tilde {});

        case '#': return make_token(Tokens::Hash {});
        case '$': return make_token(Tokens::Dollar {});

                  // double and single
        case '=': return make_token(match('=') ? TokenData(Tokens::DoubleEqual {}) : TokenData(Tokens::Equal {}));
        case ':': return make_token(match(':') ? TokenData(Tokens::DoubleColon {}) : TokenData(Tokens::Colon {}));

                  // equal and single
        case '*': return make_token(match('=') ? TokenData(Tokens::StarEqual {})    : TokenData(Tokens::Star {}));
        case '/': return make_token(match('=') ? TokenData(Tokens::SlashEqual {})   : TokenData(Tokens::Slash {}));
        case '!': return make_token(match('=') ? TokenData(Tokens::BangEqual {})    : TokenData(Tokens::Bang {}));
        case '%': return make_token(match('=') ? TokenData(Tokens::PercentEqual {}) : TokenData(Tokens::Percent {}));
        case '^': return make_token(match('=') ? TokenData(Tokens::CaretEqual {})   : TokenData(Tokens::Caret {}));

                  // double and equal and single
        case '+': return make_token(match('+') ? TokenData(Tokens::DoublePlus {})  : (match('=') ? TokenData(Tokens::PlusEqual {})  : TokenData(Tokens::Plus {})));
        case '&': return make_token(match('&') ? TokenData(Tokens::DoubleAmper {}) : (match('=') ? TokenData(Tokens::AmperEqual {}) : TokenData(Tokens::Amper {})));
        case '|': return make_token(match('|') ? TokenData(Tokens::DoublePipe {})  : (match('=') ? TokenData(Tokens::PipeEqual {})  : TokenData(Tokens::Pipe {})));

                  // arrows
        case '-': return make_token(match('-') ? TokenData(Tokens::DoubleMinus {}) : (match('=') ? TokenData(Tokens::MinusEqual {}) : (match('>') ? TokenData(Tokens::RightArrow {}) : TokenData(Tokens::Minus {}))));
        case '<': return make_token(match('<') ? (match('=') ? TokenData(Tokens::DoubleLessEqual {}) : TokenData(Tokens::DoubleLess {})) : (match('=') ? TokenData(Tokens::LessEqual {}) : (match('-') ? TokenData(Tokens::LeftArrow {}) : TokenData(Tokens::Less {}))));

                  // double, doubleequal, singleequal, single
        case '>': return make_token(match('>') ? (match('=') ? TokenData(Tokens::DoubleGreaterEqual {}) : TokenData(Tokens::DoubleGreater {})) : (match('=') ? TokenData(Tokens::GreaterEqual {}) : TokenData(Tokens::Greater {})));

        case '\'':
        case '"':
            char starting_quote = consumed();
            while (peek() != starting_quote && !at_end() && peek() != '\n')
                advance();

            if (starting_quote == '"' && peek() != '"')
                return make_token(Tokens::Error { ERR_UNTERM_STRLIT });
            else if (starting_quote == '\'' && peek() != '\'')
                return make_token(Tokens::Error { ERR_UNTERM_CHARLIT });

            advance(); // consume closing quote

            if (starting_quote == '\'') {
                if (std::distance(start, end) != 3) return make_token(Tokens::Error { ERR_MULTICHAR_CHARLIT });
                else return make_token(Tokens::CharLit { *(start + 1) });
            }

            return make_token(Tokens::StringLit { std::string(start + 1, end - 2) });
    }

    if (current >= '0' && current <= '9')
        return lex_digit(current);
    else if (is_alpha(current))
        return lex_identifier(true);

    return make_token(Tokens::Error { ERR_UNEXPECTED_CHAR });
}
// KWMATCH START {{{1
TokenData Lexer::get_identifier_type() {
    switch (*(start + 0)) {
        case 'd':
            if (std::distance(start, end) == 4 && *(start + 1) == 'a' && *(start + 2) == 't' && *(start + 3) == 'a') return Tokens::Data {  };
            break;
        case 'i':
            switch (*(start + 1)) {
                case 'm':
                    if (std::distance(start, end) == 4 && *(start + 2) == 'p' && *(start + 3) == 'l') return Tokens::Impl {  };
                    break;
                case 'f':
                    if (start + 2 == end) return Tokens::If {  };
                    break;
            }
            break;
        case 'f':
            switch (*(start + 1)) {
                case 'u':
                    if (std::distance(start, end) == 3 && *(start + 2) == 'n') return Tokens::Fun {  };
                    break;
                case 'o':
                    if (std::distance(start, end) == 3 && *(start + 2) == 'r') return Tokens::For {  };
                    break;
                case 'a':
                    if (std::distance(start, end) == 5 && *(start + 2) == 'l' && *(start + 3) == 's' && *(start + 4) == 'e') return Tokens::BoolLit { false };
                    break;
            }
            break;
        case 'v':
            if (std::distance(start, end) == 3 && *(start + 1) == 'a' && *(start + 2) == 'r') return Tokens::Var {  };
            break;
        case 'm':
            if (std::distance(start, end) == 3 && *(start + 1) == 'u' && *(start + 2) == 't') return Tokens::Mut {  };
            break;
        case 'l':
            if (std::distance(start, end) == 3 && *(start + 1) == 'e' && *(start + 2) == 't') return Tokens::Let {  };
            break;
        case 't':
            switch (*(start + 1)) {
                case 'h':
                    if (std::distance(start, end) == 4 && *(start + 2) == 'i' && *(start + 3) == 's') return Tokens::This {  };
                    break;
                case 'r':
                    if (std::distance(start, end) == 4 && *(start + 2) == 'u' && *(start + 3) == 'e') return Tokens::BoolLit { true };
                    break;
            }
            break;
        case 'r':
            if (std::distance(start, end) == 6 && *(start + 1) == 'e' && *(start + 2) == 't' && *(start + 3) == 'u' && *(start + 4) == 'r' && *(start + 5) == 'n') return Tokens::Return {  };
            break;
        case 'w':
            if (std::distance(start, end) == 5 && *(start + 1) == 'h' && *(start + 2) == 'i' && *(start + 3) == 'l' && *(start + 4) == 'e') return Tokens::While {  };
            break;
        case 'e':
            if (std::distance(start, end) == 4 && *(start + 1) == 'l' && *(start + 2) == 's' && *(start + 3) == 'e') return Tokens::Else {  };
            break;
        case 'c':
            switch (*(start + 1)) {
                case 'a':
                    if (std::distance(start, end) == 4 && *(start + 2) == 's' && *(start + 3) == 'e') return Tokens::Case {  };
                    break;
                case 'o':
                    if (std::distance(start, end) == 8 && *(start + 2) == 'n' && *(start + 3) == 't' && *(start + 4) == 'i' && *(start + 5) == 'n' && *(start + 6) == 'u' && *(start + 7) == 'e') return Tokens::Continue {  };
                    break;
            }
            break;
        case 'b':
            switch (*(start + 1)) {
                case 'r':
                    if (std::distance(start, end) == 5 && *(start + 2) == 'e' && *(start + 3) == 'a' && *(start + 4) == 'k') return Tokens::Break {  };
                    break;
                case 'o':
                    if (std::distance(start, end) == 4 && *(start + 2) == 'o' && *(start + 3) == 'm') return Tokens::Boom {  };
                    break;
            }
            break;
    }

    return Tokens::Identifier { std::string(start, end) };
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
Located<TokenData> Lexer::make_token(TokenData const &data) {
    Location tokstart (start, startline, startcolumn, sourcefile),
             tokend (end, endline, endcolumn, sourcefile);
    Span span (tokstart, tokend);
    return Located<TokenData> { span, data };
}
