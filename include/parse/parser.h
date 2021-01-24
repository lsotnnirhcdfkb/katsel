#pragma once

#include <memory>
#include "lex/token.h"

class Lexer;
struct File;
namespace ASTNS { class CUB; }

class Parser {
public:
    Parser(Lexer &l, File &sourcefile);

    std::unique_ptr<ASTNS::CUB> parse();

    Token consume();

    Lexer &lexer;
    File &sourcefile;

    bool errored;
};
