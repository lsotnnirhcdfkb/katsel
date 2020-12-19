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
#include "message/error.h"
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
    std::unique_ptr<ASTNS::CUB> parsed = parser->parse();

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

    auto codegen = std::make_unique<CodeGen>(*source);
    codegen->declarate(parsed.get());
    if (codegen->isErrored())
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
    if (codegen->isErrored())
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

        codegen->unit->cfgDot(os);

        os.close();
        return true;
    }

    auto lowerer = std::make_unique<Lower::Lowerer>(*codegen->unit);
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

    if (ofmt == OutFormats::OBJECT)
    {
        OPENFILE(filename, ".o");
        if (os.has_error())
            return false;

        if (!lowerer->objectify(os))
        {
            os.close();
            return false;
        }

        os.close();
        return true;
    }

    return true;
}

// main {{{1
int main(int argc, char *argv[])
{
    int opt;
    OutFormats ofmt = OutFormats::OBJECT;
    while ((opt = getopt(argc, argv, "f:e:")) != -1)
    {
        switch (opt)
        {
            case 'f':
#define OFMT(k, ku) \
    if (strcmp(optarg, #k) == 0) \
        ofmt = OutFormats::ku;
#define EOFMT(k, ku) else OFMT(k, ku)
                OFMT(lex, LEX)
                EOFMT(parse, PARSE)
                EOFMT(astdot, ASTDOT)
                EOFMT(decls, DECLS)
                EOFMT(codegen, CODEGEN)
                EOFMT(cfgdot, CFGDOT)
                EOFMT(lower, LOWER)
                EOFMT(object, OBJECT)
#undef OFMT
#undef EOFMT
                else
                {
                    std::cerr << "Invalid argument for option -p: '" << optarg << "'\n";
                    std::cerr << "Defaulting to -pobject\n";
                    ofmt = OutFormats::OBJECT;
                }
                break;

            case 'e':
                if (strcmp(optarg, "json") == 0)
                    errformat = ErrorFormat::JSON;
                else if (strcmp(optarg, "human") == 0)
                    errformat = ErrorFormat::HUMAN;
                else
                {
                    std::cerr << "Invalid argument for option -e: '" << optarg << "'\n";
                    std::cerr << "Defaulting to -ehuman\n";
                    errformat = ErrorFormat::HUMAN;
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
