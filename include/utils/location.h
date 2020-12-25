#pragma once

#include <string>

struct File;
struct ASTen;
namespace IR { struct ASTValue; }
namespace ASTNS { class AST; }

struct Location
{
    std::string::iterator start;
    std::string::iterator end;
    File const *file;

    Location();
    Location(Token const &t);
    Location(std::string::iterator start, std::string::iterator end, File const *file);
    Location(IR::ASTValue const &v);
    Location(IR::ASTValue const *v);
    Location(ASTNS::AST *ast);
};
