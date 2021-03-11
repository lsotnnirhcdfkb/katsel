#include "lex/lexer.h"
#include "utils/assert.h"
#include "message/errmsgs.h"

// constructors {{{1
Lexer::Lexer(File &sourcefile):
    start(sourcefile.source.begin()),
    end(start),
    startline(1),
    startcolumn(1),
    endline(1),
    endcolumn(1),
    srcstart(sourcefile.source.begin()),
    srcend(sourcefile.source.end()),
    sourcefile(sourcefile) {
    indent_stack.push_back(IndentFrame { true, 0 });
}
// lex digit and lex identifier {{{1
static bool is_alpha(char c) {
    return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c == '_';
}
void Lexer::lex_digit(char current) {
    using Base = TokenTypes::IntLit::Base; // type less

    auto is_digit = [](char const &c, Base base) {
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

    // TODO: if num_float_digits == 0, rewind, this is like when its '2.', that should be Intlit, DOT, not Errors::FloatLitNoDigits
    // TODO: any radix for number literals like smalltalk

    if (!base_valid)
        add_token(make_error_token<Errors::InvalidNumlitBase>());
    else if (!digits_valid)
        add_token(make_error_token<Errors::InvalidCharForBase>());
    else if (num_digits == 0)
        add_token(make_error_token<Errors::IntlitNoDigits>());
    else {
        if (is_float) {
            if (base != Base::DECIMAL)
                add_token(make_error_token<Errors::NondecimalFloatlit>());
            else {
                double val = stold(std::string(start, end));
                add_token(make_token(TokenTypes::FloatLit { val }));
            }
        } else {
            uint64_t val = stoll(std::string(start, end));
            add_token(make_token(TokenTypes::IntLit { val, base }));
        }
    }
}
void Lexer::lex_identifier(bool apostrophes_allowed) {
    while (is_alpha(peek()) || (peek() >= '0' && peek() <= '9')) advance();

    if (apostrophes_allowed)
        while (peek() == '\'') advance();

    Token data = get_identifier_type();
    add_token(make_token(std::move(data)));
}

// next token {{{1
Located<Token> Lexer::next_token() {
    while (token_backlog.size() == 0)
        lex_more();

    auto tok = std::move(token_backlog.front());
    token_backlog.pop();
    return tok;
}
void Lexer::add_token(Located<Token> tok) {
    token_backlog.push(std::move(tok));
}
void Lexer::lex_more() {
    // indiscriminately skip whitespace, then check if the current character is the first char of the line
    bool at_whitespace = true;
    while (at_whitespace) {
        switch (peek()) {
            case '\r':
            case ' ':
            case '\t':
            case '\n':
                advance();
                break;

            case '/':
                if (peekpeek() == '/')
                    while (peek() != '\n' && !at_end()) advance();
                else if (peekpeek() == '*') {
                    advance(); // consume '/'
                    advance(); // consume '*'
                    while (!(peek() == '*' && peekpeek() == '/') && !at_end()) {
                        advance();
                    }

                    if (at_end()) {
                        add_token(make_error_token<Errors::UntermMultilineComment>());
                        return;
                    }

                    advance(); // advance twice to consume the * and /
                    advance();
                } else
                    at_whitespace = false;
                break;

            default:
                at_whitespace = false;
                break;
        }

        if (at_end())
            at_whitespace = false;
    }

    start_to_end();

    // TODO: override, check for indentation if the last character of the last line is '~'
    if (indent_stack.back().indentation_sensitive) {
        // check if the current character is the first character of the line
        bool first_char = true;
        int indent = 0;
        for (auto iter = start - 1; iter >= srcstart && *iter != '\n'; --iter) {
            switch (*iter) {
                case ' ':
                    ++indent;
                    break;
                case '\t':
                    // round up to nearest multiple of 8:
                    indent = (indent / 8 + 1) * 8;
                    break;

                default:
                    first_char = false;
                    indent = -1;
                    break;
            }

            if (!first_char)
                break;
        }

        if (first_char) {
            // TODO: do not insert '{' or ';' if the current char is '{'
            // TODO: support \ at the end of previous line to prevent line ending insertion
            if (indent > indent_stack.back().level) {
                add_token(make_token(TokenTypes::Indent {}));
                indent_stack.push_back(IndentFrame { true, indent });
            } else if (indent < indent_stack.back().level) {
                add_token(make_token(TokenTypes::Newline {}));
                while (indent < indent_stack.back().level && indent_stack.back().indentation_sensitive) {
                    add_token(make_token(TokenTypes::Dedent {}));
                    indent_stack.pop_back();
                }

                if (indent > indent_stack.back().level && indent_stack.back().indentation_sensitive)
                    add_token(make_error_token<Errors::DedentNomatch>());
            } else { // indent == last indentation level
                if (start != srcstart) {
                    // TODO: do not insert newline if last token is ';'
                    // TODO: put newline span at last char of last line
                    add_token(make_token(TokenTypes::Newline {}));
                }
            }
        }
    }

    if (at_end()) {
        add_token(make_token(TokenTypes::_EOF {}));
        return;
    }

    char current = advance();

    switch (current) {
        case '(':
            add_token(make_token(TokenTypes::OParen {})); return;
        case ')':
            add_token(make_token(TokenTypes::CParen {})); return;
        case '[':
            add_token(make_token(TokenTypes::OBrack {})); return;
        case ']':
            add_token(make_token(TokenTypes::CBrack {})); return;
        case '{':
            add_token(make_token(TokenTypes::OBrace {}));
            indent_stack.push_back(IndentFrame { false, -1 });
            return;
        case '}':
            if (!indent_stack.back().indentation_sensitive) {
                indent_stack.pop_back();
                add_token(make_token(TokenTypes::CBrace {}));
            } else {
                add_token(make_error_token<Errors::IndentBlockCbrace>());
            }
            return;
        case ';':
            add_token(make_token(TokenTypes::Semicolon {})); return;
        case ',':
            add_token(make_token(TokenTypes::Comma {})); return;
        case '.':
            add_token(make_token(TokenTypes::Period {})); return;
        case '?':
            add_token(make_token(TokenTypes::Question {})); return;
        case '~':
            add_token(make_token(TokenTypes::Tilde {})); return;

        case '#':
            add_token(make_token(TokenTypes::Hash {})); return;
        case '$':
            add_token(make_token(TokenTypes::Dollar {})); return;

            // double and single
        case '=':
            add_token(make_token(
                match('=')
                ? Token(TokenTypes::DoubleEqual {})
                : Token(TokenTypes::Equal {})));
            return;
        case ':':
            add_token(make_token(
                match(':')
                ? Token(TokenTypes::DoubleColon {})
                : Token(TokenTypes::Colon {})));
            return;

            // equal and single
        case '*':
            add_token(make_token(
                match('=')
                ? Token(TokenTypes::StarEqual {})
                : Token(TokenTypes::Star {})));
            return;
        case '/':
            add_token(make_token(
                match('=')
                ? Token(TokenTypes::SlashEqual {})
                : Token(TokenTypes::Slash {})));
            return;
        case '!':
            add_token(make_token(
                match('=')
                ? Token(TokenTypes::BangEqual {})
                : Token(TokenTypes::Bang {})));
            return;
        case '%':
            add_token(make_token(
                match('=')
                ? Token(TokenTypes::PercentEqual {})
                : Token(TokenTypes::Percent {})));
            return;
        case '^':
            add_token(make_token(
                match('=')
                ? Token(TokenTypes::CaretEqual {})
                : Token(TokenTypes::Caret {})));
            return;

            // double and equal and single
        case '+':
            add_token(make_token(
                match('+')
                ? Token(TokenTypes::DoublePlus {})
                : match('=')
                    ? Token(TokenTypes::PlusEqual {})
                    : Token(TokenTypes::Plus {})));
            return;
        case '&':
            add_token(make_token(
                match('&')
                ? Token(TokenTypes::DoubleAmper {})
                : match('=')
                    ? Token(TokenTypes::AmperEqual {})
                    : Token(TokenTypes::Amper {})));
            return;
        case '|':
            add_token(make_token(
                match('|')
                ? Token(TokenTypes::DoublePipe {})
                : match('=')
                    ? Token(TokenTypes::PipeEqual {})
                    : Token(TokenTypes::Pipe {})));
            return;

            // arrows
        case '-':
            add_token(make_token(
                match('-')
                ? Token(TokenTypes::DoubleMinus {})
                : match('=')
                    ? Token(TokenTypes::MinusEqual {})
                    : match('>')
                        ? Token(TokenTypes::RightArrow {})
                        : Token(TokenTypes::Minus {})));
            return;
        case '<':
            add_token(make_token(
                match('<')
                ? match('=')
                    ? Token(TokenTypes::DoubleLessEqual {})
                    : Token(TokenTypes::DoubleLess {})
                : match('=')
                    ? Token(TokenTypes::LessEqual {})
                    : match('-')
                        ? Token(TokenTypes::LeftArrow {})
                        : Token(TokenTypes::Less {})));
            return;

            // double, doubleequal, singleequal, single
        case '>':
            add_token(make_token(
                match('>')
                ? match('=')
                    ? Token(TokenTypes::DoubleGreaterEqual {})
                    : Token(TokenTypes::DoubleGreater {})
                : match('=')
                    ? Token(TokenTypes::GreaterEqual {})
                    : Token(TokenTypes::Greater {})));
            return;

        case '\'':
        case '"':
            char starting_quote = consumed();
            while (peek() != starting_quote && !at_end() && peek() != '\n')
                advance();

            if (starting_quote == '"' && peek() != '"') {
                add_token(make_error_token<Errors::UntermStrlit>());
                return;
            } else if (starting_quote == '\'' && peek() != '\'') {
                add_token(make_error_token<Errors::UntermCharlit>());
                return;
            }

            advance(); // consume closing quote

            if (starting_quote == '\'') {
                if (std::distance(start, end) != 3) {
                    add_token(make_error_token<Errors::MulticharCharlit>());
                } else {
                    add_token(make_token(TokenTypes::CharLit { *(start + 1) }));
                }
            } else {
                add_token(make_token(TokenTypes::StringLit { std::string(start + 1, end - 2) }));
            }
            return;
    }

    if (current >= '0' && current <= '9')  {
        lex_digit(current);
    } else if (is_alpha(current)) {
        lex_identifier(true);
    } else {
        add_token(make_error_token<Errors::BadChar>());
    }
}
// KWMATCH START {{{1
Token Lexer::get_identifier_type() {
    switch (*(start + 0)) {
        case 'd':
            if (std::distance(start, end) == 4 && *(start + 1) == 'a' && *(start + 2) == 't' && *(start + 3) == 'a') return TokenTypes::Data {  };
            break;
        case 'i':
            switch (*(start + 1)) {
                case 'm':
                    if (std::distance(start, end) == 4 && *(start + 2) == 'p' && *(start + 3) == 'l') return TokenTypes::Impl {  };
                    break;
                case 'f':
                    if (start + 2 == end) return TokenTypes::If {  };
                    break;
            }
            break;
        case 'f':
            switch (*(start + 1)) {
                case 'u':
                    if (std::distance(start, end) == 3 && *(start + 2) == 'n') return TokenTypes::Fun {  };
                    break;
                case 'o':
                    if (std::distance(start, end) == 3 && *(start + 2) == 'r') return TokenTypes::For {  };
                    break;
                case 'a':
                    if (std::distance(start, end) == 5 && *(start + 2) == 'l' && *(start + 3) == 's' && *(start + 4) == 'e') return TokenTypes::BoolLit { false };
                    break;
            }
            break;
        case 'v':
            if (std::distance(start, end) == 3 && *(start + 1) == 'a' && *(start + 2) == 'r') return TokenTypes::Var {  };
            break;
        case 'm':
            if (std::distance(start, end) == 3 && *(start + 1) == 'u' && *(start + 2) == 't') return TokenTypes::Mut {  };
            break;
        case 'l':
            if (std::distance(start, end) == 3 && *(start + 1) == 'e' && *(start + 2) == 't') return TokenTypes::Let {  };
            break;
        case 't':
            switch (*(start + 1)) {
                case 'h':
                    if (std::distance(start, end) == 4 && *(start + 2) == 'i' && *(start + 3) == 's') return TokenTypes::This {  };
                    break;
                case 'r':
                    if (std::distance(start, end) == 4 && *(start + 2) == 'u' && *(start + 3) == 'e') return TokenTypes::BoolLit { true };
                    break;
            }
            break;
        case 'r':
            if (std::distance(start, end) == 6 && *(start + 1) == 'e' && *(start + 2) == 't' && *(start + 3) == 'u' && *(start + 4) == 'r' && *(start + 5) == 'n') return TokenTypes::Return {  };
            break;
        case 'w':
            if (std::distance(start, end) == 5 && *(start + 1) == 'h' && *(start + 2) == 'i' && *(start + 3) == 'l' && *(start + 4) == 'e') return TokenTypes::While {  };
            break;
        case 'e':
            if (std::distance(start, end) == 4 && *(start + 1) == 'l' && *(start + 2) == 's' && *(start + 3) == 'e') return TokenTypes::Else {  };
            break;
        case 'c':
            switch (*(start + 1)) {
                case 'a':
                    if (std::distance(start, end) == 4 && *(start + 2) == 's' && *(start + 3) == 'e') return TokenTypes::Case {  };
                    break;
                case 'o':
                    if (std::distance(start, end) == 8 && *(start + 2) == 'n' && *(start + 3) == 't' && *(start + 4) == 'i' && *(start + 5) == 'n' && *(start + 6) == 'u' && *(start + 7) == 'e') return TokenTypes::Continue {  };
                    break;
            }
            break;
        case 'b':
            switch (*(start + 1)) {
                case 'r':
                    if (std::distance(start, end) == 5 && *(start + 2) == 'e' && *(start + 3) == 'a' && *(start + 4) == 'k') return TokenTypes::Break {  };
                    break;
                case 'o':
                    if (std::distance(start, end) == 4 && *(start + 2) == 'o' && *(start + 3) == 'm') return TokenTypes::Boom {  };
                    break;
            }
            break;
    }

    return TokenTypes::Identifier { std::string(start, end) };
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
    if (peek() == '\n') {
        ++endline;
        endcolumn = 1;
    }

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
static Span make_span(std::string::const_iterator start, std::string::const_iterator end,
        int startline, int startcolumn,
        int endline, int endcolumn,
        bool end_inc, File const &sourcefile) {
    Location tokstart (start, startline, startcolumn, sourcefile),
     // not perfect: endcolumn should wrap around to the next line if the current character is a newline but whatever
             tokend (end_inc ? end + 1 : end, endline, end_inc ? endcolumn + 1 : endcolumn, sourcefile);

    return Span(tokstart, tokend);
}
Located<TokenData> Lexer::make_token(TokenData data) {
    Span span (make_span(start, end, startline, startcolumn, endline, endcolumn, start == end, sourcefile));
    return Located<TokenData> { span, std::move(data) };
}
template <typename T>
Located<TokenData> Lexer::make_error_token() {
    Span span (make_span(start, end, startline, startcolumn, endline, endcolumn, start == end, sourcefile));
    return Located<TokenData> { span, Tokens::Error { std::make_unique<T>(span) } };
}
