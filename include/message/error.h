#pragma once

#include "lex/tokentype.h"
#include "ir/type.h"
#include "ir/value.h"
#include "ast/ast.h"

#include <vector>
#include <sstream>

struct Location
{
    std::string::iterator start;
    std::string::iterator end;
    File const *file;

    Location(Token const &t);
    Location(std::string::iterator start, std::string::iterator end, File const *file);
    Location(IR::ASTValue const &v);
    Location(IR::ASTValue const *v);
    Location(ASTNS::AST *ast);
};

extern enum class ErrorFormat
{
    HUMAN, JSON
} errformat;
