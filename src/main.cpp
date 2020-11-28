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

#include "llvm/Support/raw_ostream.h"

// output formats {{{1
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

// read a file {{{1
std::unique_ptr<File> readFile(char *filename)
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

        return std::make_unique<File>(File {std::string(filename), contents});
    }
    else
    {
        std::perror("Could not open file");
        return nullptr;
    }
}

// open a file {{{1
// llvm::raw_fd_ostream has no move assignment operator, no move constructor
#define OPENFILE(p, extrepl)                                                                         \
    std::filesystem::path pathrepl (p);                                                              \
    pathrepl.replace_extension(extrepl);                                                             \
    std::string passtr (pathrepl.string());                                                          \
    std::error_code ec;                                                                              \
    llvm::raw_fd_ostream os (passtr, ec);                                                            \
    if (ec)                                                                                          \
        llvm::errs() << "Could not open output file \"" << passtr << "\": " << ec.message() << "\n";

// compile a file {{{1
int compileFile(OutFormats ofmt, char *filename)
{
    auto source (readFile(filename));
    if (!source)
        return false;

    auto lexer = std::make_unique<Lexer>(*source);
    if (ofmt == OutFormats::LEX)
    {
        OPENFILE(filename, ".lexed.txt");
        if (os.has_error())
            return false;

        while (true)
        {
            Token t (lexer->nextToken());
            if (t.type == TokenType::EOF_)
                break;

            os << t.sourcefile->filename << ':' << t.line << ':' << t.column << ": (" << stringifyTokenType(t.type) << ") \"" << std::string(t.start, t.end) << "\"\n";
        }

        os.close();
        return true;
    }

    auto parser = std::make_unique<Parser>(*lexer, *source);
    std::unique_ptr<ASTNS::DeclB> parsed = parser->parse();

    if (!parsed)
        return false;

    if (ofmt == OutFormats::PARSE)
    {
        OPENFILE(filename, ".parsed.txt");
        if (os.has_error())
            return false;

        auto printv = std::make_unique<ASTNS::PrintVisitor>(os);
        parsed->accept(printv.get());

        os.close();
        return true;
    }

    if (ofmt == OutFormats::ASTDOT)
    {
        OPENFILE(filename, ".dot");
        if (os.has_error())
            return false;

        auto dotter = std::make_unique<ASTNS::DotVisitor>(os);
        dotter->dotVisit(parsed.get());

        os.close();
        return true;
    }

    auto codegen = std::make_unique<CodeGenNS::CodeGen>(*source);
    codegen->declarate(parsed.get());
    if (codegen->errored)
        return false;

    if (ofmt == OutFormats::DECLS)
    {
        OPENFILE(filename, ".kslir");
        if (os.has_error())
            return false;

        codegen->printUnit(os);

        os.close();
        return true;
    }

    codegen->codegen(parsed.get());
    if (codegen->errored)
        return false;

    if (ofmt == OutFormats::CODEGEN)
    {
        OPENFILE(filename, ".kslir");
        if (os.has_error())
            return false;

        codegen->printUnit(os);
        
        os.close();
        return true;
    }

    if (ofmt == OutFormats::CFGDOT)
    {
        OPENFILE(filename, ".dot");
        if (os.has_error())
            return false;

        codegen->context.unit.cfgDot(os);

        os.close();
        return true;
    }

    auto lowerer = std::make_unique<Lower::Lowerer>(codegen->context.unit);
    lowerer->lower();
    if (lowerer->errored)
        return false;

    if (ofmt == OutFormats::LOWER)
    {
        OPENFILE(filename, ".ll");
        if (os.has_error())
            return false;

        lowerer->printMod(os);

        os.close();
        return true;
    }

    return true;
}

// main {{{1
int main(int argc, char *argv[])
{
    int opt;
    OutFormats ofmt = OutFormats::ALL;
    while ((opt = getopt(argc, argv, "f:")) != -1)
    {
        switch (opt)
        {
            case 'f':
                if (strcmp(optarg, "lex") == 0)
                    ofmt = OutFormats::LEX;
                else if (strcmp(optarg, "parse") == 0)
                    ofmt = OutFormats::PARSE;
                else if (strcmp(optarg, "astdot") == 0)
                    ofmt = OutFormats::ASTDOT;
                else if (strcmp(optarg, "decls") == 0)
                    ofmt = OutFormats::DECLS;
                else if (strcmp(optarg, "codegen") == 0)
                    ofmt = OutFormats::CODEGEN;
                else if (strcmp(optarg, "cfgdot") == 0)
                    ofmt = OutFormats::CFGDOT;
                else if (strcmp(optarg, "lower") == 0)
                    ofmt = OutFormats::LOWER;
                else if (strcmp(optarg, "all") == 0)
                    ofmt = OutFormats::ALL;
                else
                {
                    std::cerr << "Invalid argument for option -p: '" << optarg << "\'\n";
                    std::cerr << "Defaulting to -pall\n";
                    ofmt = OutFormats::ALL;
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

    bool success = true;
    for (; optind < argc; ++optind)
    {
        if (!compileFile(ofmt, argv[optind]))
            success = false;
    }

    resetTerminal();
    return success ? 0 : 1;
}
