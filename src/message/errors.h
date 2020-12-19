#pragma once

#include "message/error.h"

#include "lex/tokentype.h"
#include "ir/type.h"
#include "ir/value.h"
#include "ast/ast.h"

#include <vector>
#include <sstream>

// Error class {{{1
class Error
{
public:
    class Underline
    {
    public:
        struct Message
        {
            std::string type;
            std::string text;
            char const * const color;
        };
        Location location;
        std::vector<Message> messages;
        char ch;

        Underline& error(std::string const &message);
        Underline& warning(std::string const &message);
        Underline& note(std::string const &message);
        Underline& help(std::string const &message);
        Underline& hint(std::string const &message);
        Underline& message(std::string const &type, std::string const &message);

        Underline(Location const &location, char ch);

    private:
        Underline& addmsg(std::string const &type, char const * const color, std::string const &mesage);
    };

    enum class MsgType
    {
        ERROR,
        WARNING,
    };

    Error(MsgType type, Location const &location, std::string message);

    Error& underline(Underline const &underline);

    void report() const;

    MsgType type;
    Location location;
    std::string message;

    std::vector<Underline> underlines;

private: // things needed for report() implementation
    struct showline
    {
        const File *file;
        int line;
    };

    void printHeading() const;
    std::vector<showline> collectShowlines() const;
    int countLinePad(std::vector<showline> const &showlines) const;
    void printFileLine(std::string const &pad, File const *file) const;
    void printElipsisLine(std::string const &pad) const;
    void printLine(showline const &sl, std::string const &pad) const;
};

