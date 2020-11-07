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

    Error& primary(Location const &location);
    Error& secondary(Location const &location);
    Error& span(Location const &start, Location const &end);
private:
    std::vector<Location> primaries;
    std::vector<Location> secondaries;
    std::vector<Span> spans;
};
