#include <iostream>
#include <fstream>
#include <filesystem>
#include <memory>
#include <limits>

#include <unistd.h>
#include <cstring>

#include "ast/ast.h"
#include "parse/parser.h"
#include "utils/file.h"
#include "lex/lexer.h"
#include "message/ansistuff.h"
#include "ast/printvisitor.h"
#include "ast/dotvisitor.h"
#include "codegen/codegen.h"
#include "lower/lowerer.h"

enum class OutFormats
{
    LEX = 0,
    PARSE,
    ASTDOT,
    DECLS,
    CODEGEN,
    CFGDOT,
    LOWER,
    OBJECT,
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
        std::cerr << "Could not open file\n";
        return File{"", ""};
    }
}

int main(int argc, char *argv[])
{
    int opt;
    OutFormats outformat = OutFormats::ALL;
    while ((opt = getopt(argc, argv, "f:")) != -1)
    {
        switch (opt)
        {
            case 'f':
                if (strcmp(optarg, "lex") == 0)
                    outformat = OutFormats::LEX;
                else if (strcmp(optarg, "parse") == 0)
                    outformat = OutFormats::PARSE;
                else if (strcmp(optarg, "astdot") == 0)
                    outformat = OutFormats::ASTDOT;
                else if (strcmp(optarg, "decls") == 0)
                    outformat = OutFormats::DECLS;
                else if (strcmp(optarg, "codegen") == 0)
                    outformat = OutFormats::CODEGEN;
                else if (strcmp(optarg, "cfgdot") == 0)
                    outformat = OutFormats::CFGDOT;
                else if (strcmp(optarg, "lower") == 0)
                    outformat = OutFormats::LOWER;
                else if (strcmp(optarg, "all") == 0)
                    outformat = OutFormats::ALL;
                else
                {
                    std::cerr << "Invalid argument for option -p: '" << optarg << "\'\n";
                    std::cerr << "Defaulting to -pall\n";
                    outformat = OutFormats::ALL;
                }
                break;

            default:
                break;
        }
    }

    if (optind >= argc)
    {
        std::cerr << "No input files\n";
        return 1;
    }

    enableTerminalCodes();

    for (; optind < argc; ++optind)
    {
        auto source = std::make_unique<File>(readFile(argv[optind]));
        std::filesystem::path fpath = argv[optind];

        std::filesystem::path opath (fpath);
        char const *extrepl;

        switch (outformat)
        {
            case OutFormats::LEX:
                extrepl = ".lexed.txt";
                break;

            case OutFormats::PARSE:
                extrepl = ".parsed.txt";
                break;

            case OutFormats::ASTDOT:
            case OutFormats::CFGDOT:
                extrepl = ".dot";
                break;

            case OutFormats::DECLS:
                extrepl = ".decled.kslir";
                break;

            case OutFormats::CODEGEN:
                extrepl = ".kslir";
                break;

            case OutFormats::LOWER:
                extrepl = ".ll";
                break;

            case OutFormats::OBJECT:
            case OutFormats::ALL:
                extrepl = ".o";
                break;
        }

        opath.replace_extension(extrepl);
        std::ofstream outputstream;
        outputstream.open(opath, std::ios::out);

        if (source->filename.size() == 0)
            continue;

        auto lexer = std::make_unique<Lexer>(*source);
        if (outformat == OutFormats::LEX)
        {
            while (true)
            {
                Token t (lexer->nextToken());
                if (t.type == TokenType::EOF_)
                    break;

                outputstream << t.sourcefile->filename << ':' << t.line << ':' << t.column << ": (" << stringifyTokenType(t.type) << ") \"" << std::string(t.start, t.end) << "\"\n";
            }
            continue;
        }

        auto parser = std::make_unique<Parser>(*lexer, *source);
        std::unique_ptr<ASTNS::DeclB> parsed = parser->parse();

        if (!parsed)
            continue;

        if (outformat == OutFormats::PARSE)
        {
            auto printv = std::make_unique<ASTNS::PrintVisitor>(outputstream);
            parsed->accept(printv.get());
            continue;
        }

        if (outformat == OutFormats::ASTDOT)
        {
            auto dotter = std::make_unique<ASTNS::DotVisitor>(outputstream);
            dotter->dotVisit(parsed.get());
            continue;
        }

        auto codegen = std::make_unique<CodeGenNS::CodeGen>(*source);

        codegen->declarate(parsed.get());
        if (outformat == OutFormats::DECLS)
        {
            codegen->printUnit(outputstream);
            continue;
        }

        codegen->codegen(parsed.get());
        if (outformat == OutFormats::CODEGEN)
        {
            codegen->printUnit(outputstream);
            continue;
        }

        if (outformat == OutFormats::CFGDOT)
        {
            codegen->context.unit.cfgDot(outputstream);
            continue;
        }

        auto lowerer = std::make_unique<Lower::Lowerer>(codegen->context.unit);
        lowerer->lower();
        if (outformat == OutFormats::LOWER)
        {
            lowerer->printMod(outputstream);
            continue;
        }
    }

    resetTerminal();
    return 0;
}
