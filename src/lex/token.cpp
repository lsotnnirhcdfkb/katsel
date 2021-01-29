#include "lex/token.h"
#include "lex/tokens.h"

template class TOKEN_VARIANT_TYPE;

std::string Tokens::stringify_type(TokenData const &td) {
    return std::visit([] (auto &&arg) -> std::string {
            using T = std::decay_t<decltype(arg)>;
            return T::stringify();
        }, td);
}
