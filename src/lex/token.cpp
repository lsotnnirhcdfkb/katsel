#include "lex/token.h"

Token::Token(Span const &span, TokenData const &data): span(span), data(data) {}

std::string Token::stringify_type() const {
    return std::visit([] (auto &&arg) -> std::string {
            using T = std::decay_t<decltype(arg)>;
            return T::stringify();
        }, data);
}
