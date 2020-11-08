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
        File const &file;
        std::string::iterator start, end;
    };

    class Primary
    {
    public:
        struct Message
        {
            std::string type;
            std::string message;
            char const * const color;
        };
        Location location;
        std::vector<Message> messages;

        Primary& error(std::string const &message);
        Primary& warning(std::string const &message);
        Primary& note(std::string const &message);
        Primary& help(std::string const &message);
        Primary& hint(std::string const &message);
        Primary& message(std::string const &type, std::string const &message);

        Primary(Location const &location);

    private:
        Primary& addmsg(std::string const &type, char const * const color, std::string const &mesage);
    };

    enum class MsgType
    {
        ERROR,
        WARNING,
        INTERR
    };

    Error(MsgType type, Location const &location, std::string message);

    Error& primary(Primary const &primary);
    Error& secondary(Location const &location);
    Error& span(Location const &start, Location const &end);

    void report();
    void reportAbort [[ noreturn ]] ();

private:
    MsgType type;
    Location location;
    std::string message;

    std::vector<Primary> primaries;
    std::vector<Location> secondaries;
    std::vector<Span> spans;
};

// other reporting functions {{{1
void reportAbortNoh [[ noreturn ]] (std::string const &message);
void invalidTok [[ noreturn ]] (std::string const &name, Token const &primary);
void calledWithOpTyNEthis [[ noreturn ]] (std::string const &classN, std::string const &fnn, std::string const &opname, Value const &op);
void outOSwitchDDefaultLab [[ noreturn ]] (std::string const &fnn, Location const &highlight);
void fCalled [[ noreturn ]] (std::string const &fnn);
void outOSwitchNoh [[ noreturn ]] (std::string const &fnn);
