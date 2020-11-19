
#include <iostream>
#include <fstream>
#include <filesystem>
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
#include "visit/dotvisitor.h"
#include "codegen/codegen.h"

enum class OutFormats
{
    LEX = 0,
    PARSE,
    DOT,
    DECLS,
    CODEGEN,
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
                else if (strcmp(optarg, "dot") == 0)
                    outformat = OutFormats::DOT;
                else if (strcmp(optarg, "decls") == 0)
                    outformat = OutFormats::DECLS;
                else if (strcmp(optarg, "codegen") == 0)
                    outformat = OutFormats::CODEGEN;
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

            case OutFormats::DOT:
                extrepl = ".dot";
                break;

            case OutFormats::DECLS:
                extrepl = ".decled.ll";
                break;

            case OutFormats::CODEGEN:
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
        {
            outputstream.close();
            continue;
        }

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
            outputstream.close();
            continue;
        }

        auto parser = std::make_unique<Parser>(*lexer, *source);
        std::unique_ptr<ASTNS::DeclB> parsed = parser->parse();

        if (!parsed)
        {
            outputstream.close();
            continue;
        }

        if (outformat == OutFormats::PARSE)
        {
            auto printv = std::make_unique<PrintVisitor>(outputstream);
            parsed->accept(printv.get());
            outputstream.close();
            continue;
        }

        if (outformat == OutFormats::DOT)
        {
            auto dotter = std::make_unique<DotVisitor>(outputstream);
            dotter->dotVisit(parsed.get());
            outputstream.close();
            continue;
        }

        auto codegen = std::make_unique<CodeGenNS::CodeGen>(source->filename);

        codegen->declarate(parsed.get());
        if (outformat == OutFormats::DECLS)
        {
            codegen->printMod(outputstream);
            outputstream.close();
            continue;
        }

        codegen->codegen(parsed.get());
        if (outformat == OutFormats::CODEGEN)
        {
            codegen->printMod(outputstream);
            outputstream.close();
            continue;
        }

        outputstream.close();
    }

    resetTerminal();
    return 0;
}
