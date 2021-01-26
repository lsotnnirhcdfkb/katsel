#pragma once

#include "utils/ptr.h"
#include "utils/maybe.h"
#include "utils/location.h"

#include <string>
#include <variant>
#include <cstdint>
#include <ostream>

class Token;

namespace Tokens {
    struct OParen {};
    struct CParen {};
    struct OBrack {};
    struct CBrack {};
    struct OBrace {};
    struct CBrace {};

    struct Comma {};
    struct Period {};
    struct Semicolon {};
    struct Question {};
    struct Colon {};

    struct Bang {};
    struct Plus {};
    struct Minus {};
    struct Star {};
    struct Slash {};
    struct Percent {};
    struct Equal {};
    struct Greater {};
    struct Less {};
    struct Tilde {};
    struct Amper {};
    struct Pipe {};
    struct Caret {};
    struct Dollar {};
    struct Hash {};

    struct RightArrow {};
    struct LeftArrow {};

    struct DoublePlus {};
    struct DoubleMinus {};
    struct DoubleGreater {};
    struct DoubleLess {};
    struct DoubleAmper {};
    struct DoublePipe {};
    struct DoubleEqual {};
    struct DoubleColon {};

    struct PlusEqual {};
    struct MinusEqual {};
    struct StarEqual {};
    struct SlashEqual {};
    struct BangEqual {};
    struct GreaterEqual {};
    struct LessEqual {};
    struct PercentEqual {};
    struct DoubleLessEqual {};
    struct DoubleGreaterEqual {};
    struct AmperEqual {};
    struct PipeEqual {};
    struct CaretEqual {};

    struct Identifier {
        std::string name;
    };

    struct CharLit {
        char val;
    };
    struct StringLit {
        std::string val;
    };
    struct IntLit {
        uint64_t val;
        enum class Base { DECIMAL, OCTAL, BINARY, HEX } base;
    };
    struct FloatLit {
        double val;
    };
    struct BoolLit {
        bool val;
    };

    struct This {};
    struct Var {};
    struct Fun {};
    struct Let {};
    struct Mut {};
    struct Data {};
    struct Impl {};
    struct Return {};
    struct While {};
    struct For {};
    struct If {};
    struct Else {};
    struct Case {};
    struct Break {};
    struct Continue {};

    struct Boom {};

    struct Newline {};
    struct Indent {};
    struct Dedent {};

    struct _EOF {};
    struct Error {
        using ErrFunc = void (&)(Token const &);
        using ErrFuncField = NNPtr<void(Token const &)>;
        ErrFuncField errf;
    };
}

using TokenData = std::variant<
        Tokens::OParen, Tokens::CParen,
        Tokens::OBrack, Tokens::CBrack,
        Tokens::OBrace, Tokens::CBrace,
        Tokens::Comma,
        Tokens::Period,
        Tokens::Semicolon,
        Tokens::Question,
        Tokens::Colon,
        Tokens::Bang,
        Tokens::Plus,
        Tokens::Minus,
        Tokens::Star,
        Tokens::Slash,
        Tokens::Percent,
        Tokens::Equal,
        Tokens::Greater,
        Tokens::Less,
        Tokens::Tilde,
        Tokens::Amper,
        Tokens::Pipe,
        Tokens::Caret,
        Tokens::Dollar,
        Tokens::Hash,

        Tokens::RightArrow,
        Tokens::LeftArrow,

        Tokens::DoublePlus,
        Tokens::DoubleMinus,
        Tokens::DoubleGreater,
        Tokens::DoubleLess,
        Tokens::DoubleAmper,
        Tokens::DoublePipe,
        Tokens::DoubleEqual,
        Tokens::DoubleColon,

        Tokens::PlusEqual,
        Tokens::MinusEqual,
        Tokens::StarEqual,
        Tokens::SlashEqual,
        Tokens::BangEqual,
        Tokens::GreaterEqual,
        Tokens::LessEqual,
        Tokens::PercentEqual,
        Tokens::DoubleLessEqual,
        Tokens::DoubleGreaterEqual,
        Tokens::AmperEqual,
        Tokens::PipeEqual,
        Tokens::CaretEqual,

        Tokens::Identifier,

        Tokens::CharLit,
        Tokens::StringLit,
        Tokens::IntLit,
        Tokens::FloatLit,
        Tokens::BoolLit,

        Tokens::This,
        Tokens::Var,
        Tokens::Fun,
        Tokens::Let,
        Tokens::Mut,
        Tokens::Data,
        Tokens::Impl,
        Tokens::Return,
        Tokens::While,
        Tokens::For,
        Tokens::If,
        Tokens::Else,
        Tokens::Case,
        Tokens::Break,
        Tokens::Continue,

        Tokens::Boom,

        Tokens::Newline,
        Tokens::Indent,
        Tokens::Dedent,

        Tokens::_EOF,
        Tokens::Error
    >;

class Token {
public:
    Span span;

    TokenData data;

    template <typename T>
    constexpr bool is() const {
        return std::holds_alternative<T>(data);
    }

    template <typename T>
    constexpr T as() const {
        return std::get<T>(data);
    }

private:
    template <typename T>
    static constexpr bool always_false = false;

public:
    std::string stringify_type() const {
        return std::visit([] (auto &&arg) -> std::string {
                using T = std::decay_t<decltype(arg)>;
                     if constexpr (std::is_same_v<T, Tokens::OParen>            ) return "'('";
                else if constexpr (std::is_same_v<T, Tokens::CParen>            ) return "')'";
                else if constexpr (std::is_same_v<T, Tokens::OBrack>            ) return "'['";
                else if constexpr (std::is_same_v<T, Tokens::CBrack>            ) return "']'";
                else if constexpr (std::is_same_v<T, Tokens::OBrace>            ) return "'{'";
                else if constexpr (std::is_same_v<T, Tokens::CBrace>            ) return "'}'";
                else if constexpr (std::is_same_v<T, Tokens::Comma>             ) return "','";
                else if constexpr (std::is_same_v<T, Tokens::Period>            ) return "'.'";
                else if constexpr (std::is_same_v<T, Tokens::Semicolon>         ) return "';'";
                else if constexpr (std::is_same_v<T, Tokens::Question>          ) return "'?'";
                else if constexpr (std::is_same_v<T, Tokens::Colon>             ) return "':'";
                else if constexpr (std::is_same_v<T, Tokens::Bang>              ) return "'!'";
                else if constexpr (std::is_same_v<T, Tokens::Plus>              ) return "'+'";
                else if constexpr (std::is_same_v<T, Tokens::Minus>             ) return "'-'";
                else if constexpr (std::is_same_v<T, Tokens::Star>              ) return "'*'";
                else if constexpr (std::is_same_v<T, Tokens::Slash>             ) return "'/'";
                else if constexpr (std::is_same_v<T, Tokens::Percent>           ) return "'%'";
                else if constexpr (std::is_same_v<T, Tokens::Equal>             ) return "'='";
                else if constexpr (std::is_same_v<T, Tokens::Greater>           ) return "'>'";
                else if constexpr (std::is_same_v<T, Tokens::Less>              ) return "'<'";
                else if constexpr (std::is_same_v<T, Tokens::Tilde>             ) return "'~'";
                else if constexpr (std::is_same_v<T, Tokens::Amper>             ) return "'&'";
                else if constexpr (std::is_same_v<T, Tokens::Pipe>              ) return "'|'";
                else if constexpr (std::is_same_v<T, Tokens::Caret>             ) return "'^'";
                else if constexpr (std::is_same_v<T, Tokens::Hash>              ) return "'#'";
                else if constexpr (std::is_same_v<T, Tokens::Dollar>            ) return "'$'";
                else if constexpr (std::is_same_v<T, Tokens::LeftArrow>         ) return "'<-'";
                else if constexpr (std::is_same_v<T, Tokens::RightArrow>        ) return "'->'";
                else if constexpr (std::is_same_v<T, Tokens::DoublePlus>        ) return "'++'";
                else if constexpr (std::is_same_v<T, Tokens::DoubleMinus>       ) return "'--'";
                else if constexpr (std::is_same_v<T, Tokens::DoubleGreater>     ) return "'>>'";
                else if constexpr (std::is_same_v<T, Tokens::DoubleLess>        ) return "'<<'";
                else if constexpr (std::is_same_v<T, Tokens::DoubleAmper>       ) return "'&&'";
                else if constexpr (std::is_same_v<T, Tokens::DoublePipe>        ) return "'||'";
                else if constexpr (std::is_same_v<T, Tokens::DoubleEqual>       ) return "'=='";
                else if constexpr (std::is_same_v<T, Tokens::DoubleColon>       ) return "'::'";
                else if constexpr (std::is_same_v<T, Tokens::PlusEqual>         ) return "'+='";
                else if constexpr (std::is_same_v<T, Tokens::MinusEqual>        ) return "'-='";
                else if constexpr (std::is_same_v<T, Tokens::StarEqual>         ) return "'*='";
                else if constexpr (std::is_same_v<T, Tokens::SlashEqual>        ) return "'/='";
                else if constexpr (std::is_same_v<T, Tokens::BangEqual>         ) return "'!='";
                else if constexpr (std::is_same_v<T, Tokens::GreaterEqual>      ) return "'>='";
                else if constexpr (std::is_same_v<T, Tokens::LessEqual>         ) return "'<='";
                else if constexpr (std::is_same_v<T, Tokens::PercentEqual>      ) return "'%='";
                else if constexpr (std::is_same_v<T, Tokens::DoubleLessEqual>   ) return "'<<='";
                else if constexpr (std::is_same_v<T, Tokens::DoubleGreaterEqual>) return "'>>='";
                else if constexpr (std::is_same_v<T, Tokens::AmperEqual>        ) return "'&='";
                else if constexpr (std::is_same_v<T, Tokens::PipeEqual>         ) return "'|='";
                else if constexpr (std::is_same_v<T, Tokens::CaretEqual>        ) return "'^='";
                else if constexpr (std::is_same_v<T, Tokens::Identifier>        ) return "identifier";
                else if constexpr (std::is_same_v<T, Tokens::CharLit>           ) return "character literal";
                else if constexpr (std::is_same_v<T, Tokens::StringLit>         ) return "string literal";
                else if constexpr (std::is_same_v<T, Tokens::IntLit>            ) return "integer literal";
                else if constexpr (std::is_same_v<T, Tokens::FloatLit>          ) return "floating point literal";
                else if constexpr (std::is_same_v<T, Tokens::BoolLit>           ) return "bool literal";
                else if constexpr (std::is_same_v<T, Tokens::This>              ) return "'this'";
                else if constexpr (std::is_same_v<T, Tokens::Var>               ) return "'var'";
                else if constexpr (std::is_same_v<T, Tokens::Let>               ) return "'let'";
                else if constexpr (std::is_same_v<T, Tokens::Mut>               ) return "'mut'";
                else if constexpr (std::is_same_v<T, Tokens::Fun>               ) return "'fun'";
                else if constexpr (std::is_same_v<T, Tokens::Data>              ) return "'data'";
                else if constexpr (std::is_same_v<T, Tokens::Impl>              ) return "'impl'";
                else if constexpr (std::is_same_v<T, Tokens::Return>            ) return "'return'";
                else if constexpr (std::is_same_v<T, Tokens::While>             ) return "'while'";
                else if constexpr (std::is_same_v<T, Tokens::For>               ) return "'for'";
                else if constexpr (std::is_same_v<T, Tokens::If>                ) return "'if'";
                else if constexpr (std::is_same_v<T, Tokens::Else>              ) return "'else'";
                else if constexpr (std::is_same_v<T, Tokens::Case>              ) return "'case'";
                else if constexpr (std::is_same_v<T, Tokens::Break>             ) return "'break'";
                else if constexpr (std::is_same_v<T, Tokens::Continue>          ) return "'continue'";
                else if constexpr (std::is_same_v<T, Tokens::Boom>              ) return "boom";
                else if constexpr (std::is_same_v<T, Tokens::Newline>           ) return "newline";
                else if constexpr (std::is_same_v<T, Tokens::Indent>            ) return "indent";
                else if constexpr (std::is_same_v<T, Tokens::Dedent>            ) return "dedent";
                else if constexpr (std::is_same_v<T, Tokens::_EOF>              ) return "end of file";
                else if constexpr (std::is_same_v<T, Tokens::Error>             ) return "syntax error";
                else static_assert(always_false<T>, "Visitor to stringify token type is non-exhaustive!");
            }, data);
    }

    constexpr int index() {
        return data.index();
    }

private:
    template <typename T, size_t index = 0, bool match = std::is_same_v<T, std::variant_alternative_t<index, TokenData>>>
    struct _index_of_type {
        static constexpr int value = _index_of_type<T, index + 1>::value;
    };

    template <typename T, size_t index>
    struct _index_of_type<T, index, true> {
        static constexpr int value = index;
    };

public:
    template <typename T>
    static constexpr int index_of_type = _index_of_type<T>::value;
};

inline std::ostream& operator<<(std::ostream &os, Token const &t) {
    os << "'" << t.stringify_type() << "'";
    return os;
}
