#include "parse/parser.h"

#include "lex/tokentype.h"
#include "message/errors.h"

#include <sstream>

Parser::Parser(Lexer &l, File &sourcefile): lexer(l), sourcefile(sourcefile) {}

std::unique_ptr<ASTNS::AST> Parser::parse()
{
    struct stackitem
    {
        int state;
        bool isTok;
        union
        {
            Token t;
            std::unique_ptr<ASTNS::AST> ast;
        } data;
    };
}
