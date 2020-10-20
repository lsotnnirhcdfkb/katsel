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
    std::string::iterator start;
    std::string::iterator end;
    File *file;

    Location(Token &t);
    Location(std::string::iterator start, std::string::iterator end, File *file);

    Location(ASTNS::Expr *a);
    Location(ASTNS::Decl *a);
    Location(ASTNS::Type *a);
    Location(ASTNS::Stmt *a);
};

template <typename ... Locations>
void report(MsgType msgtype, const std::string &message, Locations ... l);
