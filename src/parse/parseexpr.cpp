#include "parser.h"
#include "ast.h"

// even though everything else is recursive descent,
// expressions are parsed with a Pratt parser, because I don't
// wnat to repeat the function call mess that was the first
// recursive descent only parser. it'll work either way
// as long as it returns a valid parse tree

std::unique_ptr<ASTNS::Expr> Parser::expr()
{
}
