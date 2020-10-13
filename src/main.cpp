
#include <iostream>
#include <fstream>
#include <memory>
#include <limits>

#include <unistd.h>
#include <cstring>

#include "parse/ast.h"
#include "parse/parser.h"
#include "utils/file.h"
#include "lex/lexer.h"
#include "message/ansistuff.h"
#include "visit/printvisitor.h"
#include "codegen/codegen.h"

enum Phases
{
    LEX = 0,
    PARSE,
    CODEGEN,
    ALL,
};

File readFile(char *filename)
{
    std::ifstream filein;
    filein.open(filename);

    if (filein.is_open())
    {
        std::string contents;

        // stole this code from https://stackoverflow.com/questions/22984956/tellg-function-give-wrong-size-of-file/22986486#22986486
        filein.ignore(std::numeric_limits<std::streamsize>::max());
        std::streamsize length = filein.gcount();
        contents.resize(length);
        filein.clear();
        filein.seekg(0, std::ios_base::beg);

        filein.read(&contents[0], contents.size());

        filein.close();

        return File{std::string(filename), contents};
    }
    else
    {
        std::cerr << "Could not open file" << std::endl;
        return File{"", ""};
    }
}

int main(int argc, char *argv[])
{
    int opt;
    int phasen = Phases::ALL;
    while ((opt = getopt(argc, argv, "p:")) != -1)
    {
        switch (opt)
        {
            case 'p':
                if (strcmp(optarg, "lex") == 0)
                    phasen = Phases::LEX;
                else if (strcmp(optarg, "parse") == 0)
                    phasen = Phases::PARSE;
                else if (strcmp(optarg, "codegen") == 0)
                    phasen = Phases::CODEGEN;
                else if (strcmp(optarg, "all") == 0)
                    phasen = Phases::ALL;
                else
                {
                    std::cerr << "Invalid argument for option -p: '" << optarg << '\'' << std::endl;
                    std::cerr << "Defaulting to -pall" << std::endl;
                    phasen = Phases::ALL;
                }
                break;

            default:
                break;
        }
    }

    if (optind >= argc)
    {
        std::cerr << "No input files" << std::endl;
        return 1;
    }

    enableTerminalCodes();

    auto source = std::make_unique<File>(readFile(argv[optind]));

    auto lexer = std::make_unique<Lexer>(*source);
    if (phasen == Phases::LEX)
    {
        Token t;
        while ((t = lexer->nextToken()).type != TokenType::EOF_)
        {
            std::cout << t.sourcefile.filename << ':' << t.line << ':' << t.column << ": (" << stringifyTokenType(t.type) << ") \"" << std::string(t.start, t.end) << "\"" << std::endl;
        }
        resetTerminal();
        return 0;
    }

    auto parser = std::make_unique<Parser>(*lexer, *source);
    std::unique_ptr<ASTNS::Program> parsed = parser->parse();

    if (phasen == Phases::PARSE) // stop at phase parse which means we don't need to do any more than parsing
    {
        auto printv = std::make_unique<PrintVisitor>();
        printv->visitProgram(parsed.get());
        resetTerminal();
        return 0;
    }

    auto codegen = std::make_unique<CodeGen>();
    codegen->visitProgram(parsed.get());

    resetTerminal();
    return 0;
}
