#pragma once

#include <string>
#include <iostream>

#include "lex/token.h"
#include "utils/file.h"
#include "parse/ast.h"
#include "message/ansistuff.h"
#include <vector>
#include <iostream>

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

// report template function {{{1
Location getLine(Location const &l);
template <typename ... Locations>
void report(MsgType msgtype, const std::string &message, Location shl, Locations ... l)
{
    std::vector<Location> underlines {{l...}};
    Location showl (getLine(shl));

    // switch msgtype {{{
    switch (msgtype)
    {
        case MsgType::ERROR:
            if (ansiCodesEnabled())
                std::cerr << A_BOLD A_FG_RED "Error" A_RESET " in " A_FG_CYAN << showl.file->filename << A_RESET " " << message << std::endl;
            else
                std::cerr << "Error in " << showl.file->filename << " " << message << std::endl;
            break;
        case MsgType::WARNING:
            if (ansiCodesEnabled())
                std::cerr << A_BOLD A_FG_MAGENTA "Warning" A_RESET " in " A_FG_CYAN << showl.file->filename << A_RESET " " << message << std::endl;
            else
                std::cerr << "Warning in " << showl.file->filename << " " << message << std::endl;
            break;
        case MsgType::DEBUG:
            if (ansiCodesEnabled())
                std::cerr << A_BOLD A_FG_GREEN "Debug message" A_RESET " in " A_FG_CYAN << showl.file->filename << A_RESET " " << message << std::endl;
            else
                std::cerr << "Debug message in " << showl.file->filename << " " << message << std::endl;
            break;
        case MsgType::INTERNALERR:
            if (ansiCodesEnabled())
                std::cerr << A_BOLD "!!! - " A_FG_RED "Internal error" A_RESET ": " A_BOLD << message << A_RESET << std::endl;
            else
                std::cerr << "!!! - Internal error: " << message << std::endl;
            std::cerr << "Aborting" << std::endl;
            std::abort();
            break;
    }
    // }}}

    if (ansiCodesEnabled())
        std::cerr << A_FG_CYAN;

    auto pl = showl.start;
    auto ul = showl.start;
    while (true)
    {
        while (*pl != '\n' && pl != showl.end)
        {
            std::cout << *pl;
            ++pl;
        }

        while (*ul != '\n' && ul != showl.end)
        {
            bool draw = false;
            for (Location &l : underlines)
            {
                if (ul >= l.start && ul < l.end)
                {
                    draw = true;
                    break;
                }
            }

            if (draw)
                std::cerr << "^";
            else
                std::cerr << " ";
            ++ul;
        }

        if (pl == showl.end)
            break;

        ++pl; // consume \n
        ++ul;
    }

    if (ansiCodesEnabled())
        std::cerr << A_RESET;
    std::cerr << std::endl;
}
