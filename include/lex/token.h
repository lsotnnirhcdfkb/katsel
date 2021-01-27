#pragma once

#include "utils/ptr.h"
#include "utils/maybe.h"
#include "utils/location.h"
#include "utils/assert.h"

#include <string>
#include <variant>
#include <cstdint>
#include <ostream>

class Token;

// Token Types {{{1
namespace Tokens {
    struct OParen {
        inline static std::string stringify() { return "'('"; }
    };
    struct CParen {
        inline static std::string stringify() { return "')'"; }
    };
    struct OBrack {
        inline static std::string stringify() { return "'['"; }
    };
    struct CBrack {
        inline static std::string stringify() { return "']'"; }
    };
    struct OBrace {
        inline static std::string stringify() { return "'{'"; }
    };
    struct CBrace {
        inline static std::string stringify() { return "'}'"; }
    };

    struct Comma {
        inline static std::string stringify() { return "','"; }
    };
    struct Period {
        inline static std::string stringify() { return "'.'"; }
    };
    struct Semicolon {
        inline static std::string stringify() { return "';'"; }
    };
    struct Question {
        inline static std::string stringify() { return "'?'"; }
    };
    struct Colon {
        inline static std::string stringify() { return "':'"; }
    };

    struct Bang {
        inline static std::string stringify() { return "'!'"; }
    };
    struct Plus {
        inline static std::string stringify() { return "'+'"; }
    };
    struct Minus {
        inline static std::string stringify() { return "'-'"; }
    };
    struct Star {
        inline static std::string stringify() { return "'*'"; }
    };
    struct Slash {
        inline static std::string stringify() { return "'/'"; }
    };
    struct Percent {
        inline static std::string stringify() { return "'%'"; }
    };
    struct Equal {
        inline static std::string stringify() { return "'='"; }
    };
    struct Greater {
        inline static std::string stringify() { return "'>'"; }
    };
    struct Less {
        inline static std::string stringify() { return "'<'"; }
    };
    struct Tilde {
        inline static std::string stringify() { return "'~'"; }
    };
    struct Amper {
        inline static std::string stringify() { return "'&'"; }
    };
    struct Pipe {
        inline static std::string stringify() { return "'|'"; }
    };
    struct Caret {
        inline static std::string stringify() { return "'^'"; }
    };
    struct Dollar {
        inline static std::string stringify() { return "'$'"; }
    };
    struct Hash {
        inline static std::string stringify() { return "'#'"; }
    };

    struct RightArrow {
        inline static std::string stringify() { return "'->'"; }
    };
    struct LeftArrow {
        inline static std::string stringify() { return "'<-'"; }
    };

    struct DoublePlus {
        inline static std::string stringify() { return "'++'"; }
    };
    struct DoubleMinus {
        inline static std::string stringify() { return "'--'"; }
    };
    struct DoubleGreater {
        inline static std::string stringify() { return "'>>'"; }
    };
    struct DoubleLess {
        inline static std::string stringify() { return "'<<'"; }
    };
    struct DoubleAmper {
        inline static std::string stringify() { return "'&&'"; }
    };
    struct DoublePipe {
        inline static std::string stringify() { return "'||'"; }
    };
    struct DoubleEqual {
        inline static std::string stringify() { return "'=='"; }
    };
    struct DoubleColon {
        inline static std::string stringify() { return "'::'"; }
    };

    struct PlusEqual {
        inline static std::string stringify() { return "'+='"; }
    };
    struct MinusEqual {
        inline static std::string stringify() { return "'-='"; }
    };
    struct StarEqual {
        inline static std::string stringify() { return "'*='"; }
    };
    struct SlashEqual {
        inline static std::string stringify() { return "'/='"; }
    };
    struct BangEqual {
        inline static std::string stringify() { return "'!='"; }
    };
    struct GreaterEqual {
        inline static std::string stringify() { return "'>='"; }
    };
    struct LessEqual {
        inline static std::string stringify() { return "'<='"; }
    };
    struct PercentEqual {
        inline static std::string stringify() { return "'%='"; }
    };
    struct DoubleLessEqual {
        inline static std::string stringify() { return "'<<='"; }
    };
    struct DoubleGreaterEqual {
        inline static std::string stringify() { return "'>>='"; }
    };
    struct AmperEqual {
        inline static std::string stringify() { return "'&='"; }
    };
    struct PipeEqual {
        inline static std::string stringify() { return "'|='"; }
    };
    struct CaretEqual {
        inline static std::string stringify() { return "'^='"; }
    };

    struct Identifier {
        inline static std::string stringify() { return "identifier"; }
        std::string name;
    };

    struct CharLit {
        inline static std::string stringify() { return "char literal"; }
        char val;
    };
    struct StringLit {
        inline static std::string stringify() { return "string literal"; }
        std::string val;
    };
    struct IntLit {
        inline static std::string stringify() { return "integer literal"; }
        uint64_t val;
        enum class Base { DECIMAL, OCTAL, BINARY, HEX } base;
    };
    struct FloatLit {
        inline static std::string stringify() { return "floating point literal"; }
        double val;
    };
    struct BoolLit {
        inline static std::string stringify() { return "bool literal"; }
        bool val;
    };

    struct This {
        inline static std::string stringify() { return "'this'"; }
    };
    struct Var {
        inline static std::string stringify() { return "'var'"; }
    };
    struct Fun {
        inline static std::string stringify() { return "'fun'"; }
    };
    struct Let {
        inline static std::string stringify() { return "'let'"; }
    };
    struct Mut {
        inline static std::string stringify() { return "'mut'"; }
    };
    struct Data {
        inline static std::string stringify() { return "'data'"; }
    };
    struct Impl {
        inline static std::string stringify() { return "'impl'"; }
    };
    struct Return {
        inline static std::string stringify() { return "'return'"; }
    };
    struct While {
        inline static std::string stringify() { return "'while'"; }
    };
    struct For {
        inline static std::string stringify() { return "'for'"; }
    };
    struct If {
        inline static std::string stringify() { return "'if'"; }
    };
    struct Else {
        inline static std::string stringify() { return "'else'"; }
    };
    struct Case {
        inline static std::string stringify() { return "'case'"; }
    };
    struct Break {
        inline static std::string stringify() { return "'break'"; }
    };
    struct Continue {
        inline static std::string stringify() { return "'continue'"; }
    };

    struct Boom {
        inline static std::string stringify() { return "boom"; }
    };

    struct Newline {
        inline static std::string stringify() { return "newline"; }
    };
    struct Indent {
        inline static std::string stringify() { return "indent"; }
    };
    struct Dedent {
        inline static std::string stringify() { return "dedent"; }
    };

    struct _EOF {
        inline static std::string stringify() { return "end of file"; }
    };
    struct Error {
        inline static std::string stringify() { return "syntax error"; }
        using ErrFunc = void (&)(Token const &);
        using ErrFuncField = NNPtr<void(Token const &)>;
        ErrFuncField errf;
    };
}
// TokenData type alias {{{1
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
// Token class {{{1
class Token {
public:
    Span span;

    TokenData data;

    Token(Span const &span, TokenData const &data);

    template <typename T>
    constexpr bool is() const {
        return std::holds_alternative<T>(data);
    }
    template <typename T>
    constexpr T as() const {
        ASSERT(is<T>());
        return std::get<T>(data);
    }

private:
    template <typename T, size_t index = 0, bool match = std::is_same_v<T, std::variant_alternative_t<index, TokenData>>>
    struct _index_of_type { static constexpr int value = _index_of_type<T, index + 1>::value; };
    template <typename T, size_t index>
    struct _index_of_type<T, index, true> { static constexpr int value = index; };

public:
    template <typename T>
    static constexpr int index_of = _index_of_type<T>::value;

    std::string stringify_type() const;

    constexpr int index() {
        return data.index();
    }
};

inline std::ostream& operator<<(std::ostream &os, Token const &t) {
    os << "'" << t.stringify_type() << "'";
    return os;
}
