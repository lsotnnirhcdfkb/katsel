#pragma once

#include <string>
#include <cstdint>

#include "utils/ptr.h"
class Span;

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
        using ErrFunc = void (&)(Span const &);
        using ErrFuncField = NNPtr<void(Span const &)>;
        ErrFuncField errf;
    };
}
