#pragma once

#include "lex/tokentype.h"
#include "typing/type.h"
#include "value/value.h"
#include "parse/ast.h"

#include <vector>

// Location struct {{{1
struct Location
{
    std::string::iterator start;
    std::string::iterator end;
    File const *file;

    Location(Token const &t);
    Location(std::string::iterator start, std::string::iterator end, File const *file);
    Location(Value const &v);

    Location(ASTNS::Expr *a);
    Location(ASTNS::Decl *a);
    Location(ASTNS::Type *a);
    Location(ASTNS::Stmt *a);
};
// Error class {{{1
class Error
{
public:
    struct Span
    {
        Location start;
        Location end;
    };

    class Primary
    {
        struct Message
        {
            std::string type;
            std::string message;
            char const * const color;
        };
    public:
        Location location;
        std::vector<Message> messages;

        Primary& error(std::string const &message);
        Primary& warning(std::string const &message);
        Primary& note(std::string const &message);
        Primary& help(std::string const &message);
        Primary& hint(std::string const &message);
        Primary& message(std::string const &type, std::string const &message);

    private:
        Primary& addmsg(std::string const &type, char const * const color, std::string const &mesage);
    };

    Error& primary(Primary const &primary);
    Error& secondary(Location const &location);
    Error& span(Location const &start, Location const &end);

private:
    std::vector<Primary> primaries;
    std::vector<Location> secondaries;
    std::vector<Span> spans;
};
