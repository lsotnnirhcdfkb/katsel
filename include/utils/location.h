#pragma once

#include <string>
#include "utils/ptr.h"

struct File;
namespace ASTNS { class AST; }
namespace IR { struct ASTValue; }
class Token;

class Location {
public:
    std::string::const_iterator iter;
    int line, column;
    NNPtr<File const> file;

    Location(std::string::iterator start, std::string::iterator end, int line, int column, NNPtr<File const> file);
};

class Span {
public:
    Location start;
    Location end;
    NNPtr<File const> file;

    Span(Location const &start, Location const &end);

    Span(IR::ASTValue const &v);
    Span(ASTNS::AST const &v);

    inline std::string stringify() const {
        return std::string(start.iter, end.iter);
    }
};

std::ostream& operator<<(std::ostream &os, Location const &loc);
std::ostream& operator<<(std::ostream &os, Span const &span);
