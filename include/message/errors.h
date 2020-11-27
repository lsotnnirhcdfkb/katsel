#pragma once

#include "lex/tokentype.h"
#include "ir/type.h"
#include "ir/value.h"
#include "ast/ast.h"

#include <vector>
#include <sstream>

// Location struct {{{1
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
// Error class {{{1
class Error
{
public:
    void report() const;
    void reportAbort [[ noreturn ]] ();

    // ERRORFRIENDS START

// The following code was autogenerated - see the utils/ directory
    friend void E0000(Token const &tok);
    friend void E0001(Token const &tok);
    friend void E0002(Token const &tok);
    friend void E0003(Token const &tok);
    friend void E0004(Token const &tok);
    friend void E0100(std::string const &justparsed, std::string const &expected, std::string const &whileparsing, Token const &last, Token const &lookahead);
    friend void E0101(std::string const &justparsed, std::string const &expected, Token const &last, Token const &lookahead);
    friend void E0102(std::string const &justparsed, std::string const &expected, std::string const &whileparsing, Token const &last, Token const &lookahead, std::string const &bestfix);
    friend void E0103(std::string const &justparsed, std::string const &expected, Token const &last, Token const &lookahead, std::string const &bestfix);
    friend void E0104(std::string const &justparsed, std::string const &expected, std::string const &whileparsing, Token const &last, Token const &lookahead);
    friend void E0105(std::string const &justparsed, std::string const &expected, Token const &last, Token const &lookahead);
// This code was autogenerated - see the utils/ directory

    // ERRORFRIENDS END
private:
    struct Span
    {
        File const &file;
        std::string::iterator start, end;
    };

    class Underline
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
        INTERR
    };

    Error(MsgType type, Location const &location, std::string message);

    Error& underline(Underline const &underline);
    Error& span(Location const &start, Location const &end);

    MsgType type;
    Location location;
    std::string message;

    std::vector<Underline> underlines;
    std::vector<Span> spans;
};

// other reporting functions {{{1
void reportAbortNoh [[ noreturn ]] (std::string const &message);
void invalidTok [[ noreturn ]] (std::string const &name, Token const &primary);
void calledWithOpTyNEthis [[ noreturn ]] (std::string const &classN, std::string const &fnn, std::string const &opname, IR::ASTValue const op);
void outOSwitchDDefaultLab [[ noreturn ]] (std::string const &fnn, Location const &highlight);
void fCalled [[ noreturn ]] (std::string const &fnn);
void outOSwitchNoh [[ noreturn ]] (std::string const &fnn);

inline void _concatMsg(std::stringstream &) {}

template <typename F, typename ... T>
inline void _concatMsg(std::stringstream &ss, F first, T ... others)
{
    ss << first;
    _concatMsg(ss, others...);
}

template <typename ... T>
inline std::string concatMsg(T ... items)
{
    std::stringstream ss;
    _concatMsg(ss, items...);
    return ss.str();
}
