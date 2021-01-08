#pragma once

#include "message/error.h"

#include "lex/tokentype.h"
#include "ir/type.h"
#include "ir/value.h"
#include "ast/ast.h"
#include "utils/location.h"

#include <vector>
#include <sstream>

struct Message {
    std::string type;
    std::string text;
    char const * const color;
};

class Underline {
public:
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

enum class MsgType {
    ERROR,
    WARNING,
};

class Error {
public:
    Error(MsgType type, Location const &location, std::string const &category, std::string const &code, std::string const &name);

    Error& underline(Underline const &underline);

    void report() const;

    MsgType type;
    Location location;

    std::string const category;
    std::string const code;
    std::string const name;

    std::vector<Underline> underlines;
};

