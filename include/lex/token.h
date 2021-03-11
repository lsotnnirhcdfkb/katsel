#pragma once

#include "utils/ptr.h"
#include "utils/maybe.h"
#include "utils/location.h"
#include "utils/assert.h"

#include <string>
#include <variant>
#include <memory>

namespace TokenTypes {
    struct OParen { };
    struct CParen { };
    struct OBrack { };
    struct CBrack { };

    struct Comma { };
    struct Period { };
    struct Question { };
    struct Colon { };

    struct Bang { };
    struct Plus { };
    struct Minus { };
    struct Star { };
    struct Slash { };
    struct Percent { };
    struct Equal { };
    struct Greater { };
    struct Less { };
    struct Tilde { };
    struct Amper { };
    struct Pipe { };
    struct Caret { };
    struct Dollar { };
    struct Hash { };

    struct RightArrow { };
    struct LeftArrow { };

    struct DoublePlus { };
    struct DoubleMinus { };
    struct DoubleGreater { };
    struct DoubleLess { };
    struct DoubleAmper { };
    struct DoublePipe { };
    struct DoubleEqual { };
    struct DoubleColon { };

    struct PlusEqual { };
    struct MinusEqual { };
    struct StarEqual { };
    struct SlashEqual { };
    struct BangEqual { };
    struct GreaterEqual { };
    struct LessEqual { };
    struct PercentEqual { };
    struct DoubleLessEqual { };
    struct DoubleGreaterEqual { };
    struct AmperEqual { };
    struct PipeEqual { };
    struct CaretEqual { };

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

    struct This { };
    struct Var { };
    struct Fun { };
    struct Let { };
    struct Mut { };
    struct Data { };
    struct Impl { };
    struct Return { };
    struct While { };
    struct For { };
    struct If { };
    struct Else { };
    struct Case { };
    struct Break { };
    struct Continue { };

    struct Boom { };

    struct OBrace { };
    struct CBrace { };
    struct Semicolon { };

    struct Indent { };
    struct Dedent { };
    struct Newline { };

    struct _EOF { };
    struct Error {
        using ErrFunc = void (&)(Span const &);
        using ErrFuncField = NNPtr<void(Span const &)>;
        ErrFuncField errf;
    };
}

#define TOKEN_TYPES(mac) \
    mac(OParen) \
    mac(CParen) \
    mac(OBrack) \
    mac(CBrack) \
    mac(Comma) \
    mac(Period) \
    mac(Question) \
    mac(Colon) \
    mac(Bang) \
    mac(Plus) \
    mac(Minus) \
    mac(Star) \
    mac(Slash) \
    mac(Percent) \
    mac(Equal) \
    mac(Greater) \
    mac(Less) \
    mac(Tilde) \
    mac(Amper) \
    mac(Pipe) \
    mac(Caret) \
    mac(Dollar) \
    mac(Hash) \
    mac(RightArrow) \
    mac(LeftArrow) \
    mac(DoublePlus) \
    mac(DoubleMinus) \
    mac(DoubleGreater) \
    mac(DoubleLess) \
    mac(DoubleAmper) \
    mac(DoublePipe) \
    mac(DoubleEqual) \
    mac(DoubleColon) \
    mac(PlusEqual) \
    mac(MinusEqual) \
    mac(StarEqual) \
    mac(SlashEqual) \
    mac(BangEqual) \
    mac(GreaterEqual) \
    mac(LessEqual) \
    mac(PercentEqual) \
    mac(DoubleLessEqual) \
    mac(DoubleGreaterEqual) \
    mac(AmperEqual) \
    mac(PipeEqual) \
    mac(CaretEqual) \
    mac(Identifier) \
    mac(CharLit) \
    mac(StringLit) \
    mac(IntLit) \
    mac(FloatLit) \
    mac(BoolLit) \
    mac(This) \
    mac(Var) \
    mac(Fun) \
    mac(Let) \
    mac(Mut) \
    mac(Data) \
    mac(Impl) \
    mac(Return) \
    mac(While) \
    mac(For) \
    mac(If) \
    mac(Else) \
    mac(Case) \
    mac(Break) \
    mac(Continue) \
    mac(Boom) \
    mac(OBrace) \
    mac(CBrace) \
    mac(Semicolon) \
    mac(Indent) \
    mac(Dedent) \
    mac(Newline) \
    mac(_EOF) \
    mac(Error)

enum class TokenType {
#define ENUM_ITEM(name) name,
    TOKEN_TYPES(ENUM_ITEM)
#undef ENUM_ITEM
};

namespace token_detail {
    template <TokenType ty> struct _TypeOfTy;

#define TYPEOFTYSPECIALIZATION(name) \
    template <> struct _TypeOfTy<TokenType::name> { using Ty = TokenTypes::name; };
    TOKEN_TYPES(TYPEOFTYSPECIALIZATION)
#undef TYPEOFTYSPECIALIZATION

    template <TokenType ty>
    using TypeOfTy = typename _TypeOfTy<ty>::Ty;
}

class Token {
public:
    // each token type has its own constructor
#define CONSTRUCTOR(name) Token(TokenTypes::name);
    TOKEN_TYPES(CONSTRUCTOR)
#undef CONSTRUCTOR

    // the destructor is supposed to be defaulted anyway
    // the destructor is here so that compiler will not try to create a default destructor for Token,
    //     which it can't because it needs to make a destructor for TokenImpl, which is (and
    //     should be) incomplete from this header file
    ~Token();

    // this would already be deleted anyway because of the unique_ptr field below
    Token(Token const &) = delete;

    // need explicit move constructor because explicit destructor
    // this move constructor is supposed to be default anyway, but it cannot be here because that
    //     would try to make a move constructor for std::unique_ptr<TokenImpl>, which needs to make
    //     a destructor for TokenImpl
    Token(Token &&);

    // deleted copy assignment operator
    Token &operator=(Token const &) = delete;

    // need explicit move assignment operator for the same reason as the explicit move constructor
    // this should be defaulted, but can't here for the same reason as the explicit move constructor
    Token &operator=(Token &&);

    template <TokenType ty>
    bool is() const;

    template <TokenType ty>
    token_detail::TypeOfTy<ty> const &as() const;

    TokenType type() const;

    std::string stringify_type() const;

    Token clone() const;

private:
    class TokenImpl;

    std::unique_ptr<TokenImpl> pimpl;

    Token(TokenImpl &other);
};

#ifndef KEEP_TOKEN_TYPES_MACRO
#undef TOKEN_TYPES
#endif
