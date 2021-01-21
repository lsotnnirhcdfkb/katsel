#pragma once

#include "lex/tokentype.h"
#include "utils/file.h"
#include "utils/ptr.h"
#include "utils/maybe.h"

#include <string>
#include <ostream>

struct Token {
    TokenType type;
    std::string::iterator start;
    std::string::iterator end;

    using ErrFunc = NNPtr<void (Token const &)>;
    Maybe<ErrFunc> errf;

    int line;
    int column;

    NNPtr<File const> sourcefile;

    inline std::string stringify() const {
        return std::string(start, end);
    }

    friend std::ostream& operator<<(std::ostream &, Token const &);
};

inline std::ostream& operator<<(std::ostream &os, Token const &t) {
    os << "'" << t.stringify() << "'";
    return os;
}
inline std::ostream& operator<<(std::ostream &os, TokenType const &t) {
    os << stringifyTokenType(t);
    return os;
}
