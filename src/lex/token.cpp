#include "lex/token.h"
#include "lex/tokens.h"

std::string Tokens::stringify_type(TokenData const &td) const {
    return std::visit([] (auto &&arg) -> std::string {
            using T = std::decay_t<decltype(arg)>;
            return T::stringify();
        }, td);
}
