#pragma once

#include "lex/tokentype.h"
#include "utils/file.h"
#include <string>
#include <ostream>

struct Token {
    TokenType type;
    std::string::iterator start;
    std::string::iterator end;

    void (*errf)(Token const &);

    int line;
    int column;

    File *sourcefile;

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
