#pragma once

#include <string>
#include <iostream>

#include "lex/token.h"
#include "utils/file.h"
#include "parse/ast.h"

enum class MsgType
{
    ERROR,
    WARNING,
    DEBUG,
    INTERNALERR
};

struct Location
{
    std::string::iterator const start;
    std::string::iterator const end;
    File const &file;

    Location(Token &t);

    Location(std::string::iterator const start, std::string::iterator const end, File const &file);

    Location(ASTNS::Expr *e);
    Location(ASTNS::Decl *e);
    Location(ASTNS::Type *e);
    Location(ASTNS::Param *e);
    Location(ASTNS::Arg *e);
};

template <typename ... Locations>
void report(MsgType msgtype, const std::string &message, Locations ... l);

template <typename ... Whatever>
void reportError(Whatever ... whatever);
