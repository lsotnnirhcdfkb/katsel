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
        add_token(make_token(Tokens::Error { ERR_INVALID_NUMLIT_BASE }));
    else if (!digits_valid)
        add_token(make_token(Tokens::Error { ERR_INVALID_CHAR_FOR_BASE }));
    else if (num_digits == 0)
        add_token(make_token(Tokens::Error { ERR_INTLIT_NO_DIGITS }));
    else {
        if (is_float) {
            if (base != Base::DECIMAL)
                add_token(make_token(Tokens::Error { ERR_NONDECIMAL_FLOATLIT }));
            else {
                double val = stold(std::string(start, end));
                add_token(make_token(Tokens::FloatLit { val }));
            }
        } else {
            uint64_t val = stoll(std::string(start, end));
            add_token(make_token(Tokens::IntLit { val, base }));
        }
    }
}
void Lexer::lex_identifier(bool apostrophes_allowed) {
    while (is_alpha(peek()) || (peek() >= '0' && peek() <= '9')) advance();

    if (apostrophes_allowed)
        while (peek() == '\'') advance();

    TokenData data = get_identifier_type();
    add_token(make_token(data));
}
// next token {{{1
Located<TokenData> Lexer::next_token() {
    while (token_backlog.size() == 0)
        lex_more();

    auto tok = token_backlog.front();
    token_backlog.pop();
    return tok;
}
void Lexer::add_token(Located<TokenData> const &tok) {
    token_backlog.push(tok);
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
                        add_token(make_token(Tokens::Error { ERR_UNTERM_MULTILINE_COMMENT }));
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
                add_token(make_token(Tokens::Indent {}));
                indent_stack.push_back(IndentFrame { true, indent });
            } else if (indent < indent_stack.back().level) {
                add_token(make_token(Tokens::Newline {}));
                while (indent < indent_stack.back().level && indent_stack.back().indentation_sensitive) {
                    // TODO: do not pop if empty
                    add_token(make_token(Tokens::Dedent {}));
                    indent_stack.pop_back();
                }

                if (indent > indent_stack.back().level && indent_stack.back().indentation_sensitive)
                    add_token(make_token(Tokens::Error { ERR_DEDENT_NOMATCH }));
            } else { // indent == last indentation level
                if (start != srcstart) {
                    // TODO: do not insert newline if last token is ';'
                    // TODO: put newline span at last char of last line
                    add_token(make_token(Tokens::Newline {}));
                }
            }
        }
    }

    if (at_end()) {
        add_token(make_token(Tokens::_EOF {}));
        return;
    }

    char current = advance();

    switch (current) {
        case '(':
            add_token(make_token(Tokens::OParen {})); return;
        case ')':
            add_token(make_token(Tokens::CParen {})); return;
        case '[':
            add_token(make_token(Tokens::OBrack {})); return;
        case ']':
            add_token(make_token(Tokens::CBrack {})); return;
        case '{':
            add_token(make_token(Tokens::OBrace {}));
            indent_stack.push_back(IndentFrame { false, -1 });
            return;
        case '}':
            if (!indent_stack.back().indentation_sensitive) {
                // TODO: do not pop if empty
                indent_stack.pop_back();
                add_token(make_token(Tokens::CBrace {}));
            } else {
                add_token(make_token(Tokens::Error { ERR_INDENT_BLOCK_CBRACE }));
            }
            return;
        case ';':
            add_token(make_token(Tokens::Semicolon {})); return;
        case ',':
            add_token(make_token(Tokens::Comma {})); return;
        case '.':
            add_token(make_token(Tokens::Period {})); return;
        case '?':
            add_token(make_token(Tokens::Question {})); return;
        case '~':
            add_token(make_token(Tokens::Tilde {})); return;

        case '#':
            add_token(make_token(Tokens::Hash {})); return;
        case '$':
            add_token(make_token(Tokens::Dollar {})); return;

            // double and single
        case '=':
            add_token(make_token(
                match('=')
                ? TokenData(Tokens::DoubleEqual {})
                : TokenData(Tokens::Equal {})));
            return;
        case ':':
            add_token(make_token(
                match(':')
                ? TokenData(Tokens::DoubleColon {})
                : TokenData(Tokens::Colon {})));
            return;

            // equal and single
        case '*':
            add_token(make_token(
                match('=')
                ? TokenData(Tokens::StarEqual {})
                : TokenData(Tokens::Star {})));
            return;
        case '/':
            add_token(make_token(
                match('=')
                ? TokenData(Tokens::SlashEqual {})
                : TokenData(Tokens::Slash {})));
            return;
        case '!':
            add_token(make_token(
                match('=')
                ? TokenData(Tokens::BangEqual {})
                : TokenData(Tokens::Bang {})));
            return;
        case '%':
            add_token(make_token(
                match('=')
                ? TokenData(Tokens::PercentEqual {})
                : TokenData(Tokens::Percent {})));
            return;
        case '^':
            add_token(make_token(
                match('=')
                ? TokenData(Tokens::CaretEqual {})
                : TokenData(Tokens::Caret {})));
            return;

            // double and equal and single
        case '+':
            add_token(make_token(
                match('+')
                ? TokenData(Tokens::DoublePlus {})
                : match('=')
                    ? TokenData(Tokens::PlusEqual {})
                    : TokenData(Tokens::Plus {})));
            return;
        case '&':
            add_token(make_token(
                match('&')
                ? TokenData(Tokens::DoubleAmper {})
                : match('=')
                    ? TokenData(Tokens::AmperEqual {})
                    : TokenData(Tokens::Amper {})));
            return;
        case '|':
            add_token(make_token(
                match('|')
                ? TokenData(Tokens::DoublePipe {})
                : match('=')
                    ? TokenData(Tokens::PipeEqual {})
                    : TokenData(Tokens::Pipe {})));
            return;

            // arrows
        case '-':
            add_token(make_token(
                match('-')
                ? TokenData(Tokens::DoubleMinus {})
                : match('=')
                    ? TokenData(Tokens::MinusEqual {})
                    : match('>')
                        ? TokenData(Tokens::RightArrow {})
                        : TokenData(Tokens::Minus {})));
            return;
        case '<':
            add_token(make_token(
                match('<')
                ? match('=')
                    ? TokenData(Tokens::DoubleLessEqual {})
                    : TokenData(Tokens::DoubleLess {})
                : match('=')
                    ? TokenData(Tokens::LessEqual {})
                    : match('-')
                        ? TokenData(Tokens::LeftArrow {})
                        : TokenData(Tokens::Less {})));
            return;

            // double, doubleequal, singleequal, single
        case '>':
            add_token(make_token(
                match('>')
                ? match('=')
                    ? TokenData(Tokens::DoubleGreaterEqual {})
                    : TokenData(Tokens::DoubleGreater {})
                : match('=')
                    ? TokenData(Tokens::GreaterEqual {})
                    : TokenData(Tokens::Greater {})));
            return;

        case '\'':
        case '"':
            char starting_quote = consumed();
            while (peek() != starting_quote && !at_end() && peek() != '\n')
                advance();

            if (starting_quote == '"' && peek() != '"') {
                add_token(make_token(Tokens::Error { ERR_UNTERM_STRLIT }));
                return;
            } else if (starting_quote == '\'' && peek() != '\'') {
                add_token(make_token(Tokens::Error { ERR_UNTERM_CHARLIT }));
                return;
            }

            advance(); // consume closing quote

            if (starting_quote == '\'') {
                if (std::distance(start, end) != 3) {
                    add_token(make_token(Tokens::Error { ERR_MULTICHAR_CHARLIT }));
                } else {
                    add_token(make_token(Tokens::CharLit { *(start + 1) }));
                }
            } else {
                add_token(make_token(Tokens::StringLit { std::string(start + 1, end - 2) }));
            }
            return;
    }

    if (current >= '0' && current <= '9')  {
        lex_digit(current);
    } else if (is_alpha(current)) {
        lex_identifier(true);
    } else {
        add_token(make_token(Tokens::Error { ERR_BAD_CHAR }));
    }
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
Located<TokenData> Lexer::make_token(TokenData const &data) {
    bool end_inc = start == end;
    Location tokstart (start, startline, startcolumn, sourcefile),
     // not perfect: endcolumn should wrap around to the next line if the current character is a newline but whatever
             tokend (end_inc ? end + 1 : end, endline, end_inc ? endcolumn + 1 : endcolumn, sourcefile);
    Span span (tokstart, tokend);
    return Located<TokenData> { span, data };
}
