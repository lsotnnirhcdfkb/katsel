#pragma once

#include "lex/tokentype.h"
#include "utils/ptr.h"
#include "utils/maybe.h"

struct File;

#include <string>
#include <ostream>

struct Token {
    TokenType type;
    std::string::iterator start;
    std::string::iterator end;

    using ErrFunc = void (&)(Token const &);
    using ErrFuncField = NNPtr<void(Token const &)>;
    Maybe<ErrFuncField> errf;

    int line;
    int column;

    NNPtr<File const> sourcefile;

    inline std::string stringify() const {
        return std::string(start, end);
    }
};

inline std::ostream& operator<<(std::ostream &os, Token const &t) {
    os << "'" << t.stringify() << "'";
    return os;
}
inline std::ostream& operator<<(std::ostream &os, TokenType const &t) {
    os << stringify_token_type(t);
    return os;
}
