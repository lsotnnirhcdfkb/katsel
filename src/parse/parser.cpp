#include <memory>

#include "parse/parser.h"
#include "lex/lexer.h"
#include "ast/ast.h"
#include "message/errmsgs.h"

class Parser {
public:
    Parser(Lexer &l, File &source): lexer(l), source(source) {}

    std::unique_ptr<ASTNS::CUB> parse() {

    }

    Lexer &lexer;
    File &source;
};

std::unique_ptr<ASTNS::CUB> parse(Lexer &l, File &sourcefile) {
    Parser p (l, sourcefile);
    return p.parse();
}
