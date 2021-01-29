#pragma once

#include "utils/ptr.h"
#include "utils/maybe.h"
#include "utils/location.h"
#include "utils/assert.h"
#include "lex/tokens.h"

#include <string>
#include <variant>

#define TOKEN_VARIANT_TYPE std::variant<Tokens::OParen, Tokens::CParen, Tokens::OBrack, Tokens::CBrack, Tokens::OBrace, Tokens::CBrace, Tokens::Comma, Tokens::Period, Tokens::Semicolon, Tokens::Question, Tokens::Colon, Tokens::Bang, Tokens::Plus, Tokens::Minus, Tokens::Star, Tokens::Slash, Tokens::Percent, Tokens::Equal, Tokens::Greater, Tokens::Less, Tokens::Tilde, Tokens::Amper, Tokens::Pipe, Tokens::Caret, Tokens::Dollar, Tokens::Hash, Tokens::RightArrow, Tokens::LeftArrow, Tokens::DoublePlus, Tokens::DoubleMinus, Tokens::DoubleGreater, Tokens::DoubleLess, Tokens::DoubleAmper, Tokens::DoublePipe, Tokens::DoubleEqual, Tokens::DoubleColon, Tokens::PlusEqual, Tokens::MinusEqual, Tokens::StarEqual, Tokens::SlashEqual, Tokens::BangEqual, Tokens::GreaterEqual, Tokens::LessEqual, Tokens::PercentEqual, Tokens::DoubleLessEqual, Tokens::DoubleGreaterEqual, Tokens::AmperEqual, Tokens::PipeEqual, Tokens::CaretEqual, Tokens::Identifier, Tokens::CharLit, Tokens::StringLit, Tokens::IntLit, Tokens::FloatLit, Tokens::BoolLit, Tokens::This, Tokens::Var, Tokens::Fun, Tokens::Let, Tokens::Mut, Tokens::Data, Tokens::Impl, Tokens::Return, Tokens::While, Tokens::For, Tokens::If, Tokens::Else, Tokens::Case, Tokens::Break, Tokens::Continue, Tokens::Boom, Tokens::Newline, Tokens::Indent, Tokens::Dedent, Tokens::_EOF, Tokens::Error>
extern template class TOKEN_VARIANT_TYPE;
using TokenData = TOKEN_VARIANT_TYPE;

namespace Tokens {
    namespace _iot {
        template <typename T, size_t index = 0, bool match = std::is_same_v<T, std::variant_alternative_t<index, TokenData>>>
        struct _index_of_type { static constexpr int value = _index_of_type<T, index + 1>::value; };
        template <typename T, size_t index>
        struct _index_of_type<T, index, true> { static constexpr int value = index; };
    }

    template <typename T>
    static constexpr int index_of = _iot::_index_of_type<T>::value;

    template <typename T>
    constexpr bool is(TokenData const &td) {
        return std::holds_alternative<T>(td);
    }
    template <typename T>
    constexpr T as(TokenData const &td) {
        ASSERT(is<T>(td))
        return std::get<T>(td);
    }

    std::string stringify_type(TokenData const &td);
}
