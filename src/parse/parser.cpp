#include "parse/parser.h"

#include "lex/tokentype.h"
#include "message/errors.h"

#include <sstream>

// Parser constructor {{{1
Parser::Parser(Lexer &l, File &sourcefile): lexer(l), sourcefile(sourcefile), ispanic(false)
{
    consume();
}
