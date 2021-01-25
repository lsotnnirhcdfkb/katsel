#pragma once

#include <string>
#include "utils/ptr.h"

struct File;
struct ASTen;
namespace IR { struct ASTValue; }
namespace ASTNS { class AST; }
struct Token;

struct Location {
    std::string::iterator start;
    std::string::iterator end;
    NNPtr<File const> file;

    Location(Token const &t);
    Location(std::string::iterator start, std::string::iterator end, NNPtr<File const> file);
    Location(IR::ASTValue const &v);
    Location(NNPtr<IR::ASTValue const> v);
    Location(NNPtr<ASTNS::AST> v);
    Location(ASTNS::AST const &v);

    template <typename T>
    Location(NNPtr<T> ast): Location(NNPtr<ASTNS::AST>(ast)) {}
};
