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
#include "utils/ptr.h"
#include "utils/format.h"
#include "lex/lexer.h"
#include "message/ansistuff.h"
#include "message/error.h"
#include "ast/printvisitor.h"
#include "codegen/codegen.h"
#include "lower/lowerer.h"

#include "llvm/Support/raw_ostream.h"

// output formats {{{1
enum class OutFormats {
    LEX = 0,
    PARSE,
    CODEGEN,
    LOWER,
    OBJECT,
};

// read a file {{{1
std::unique_ptr<File> read_file(NNPtr<char> filename) {
    std::ifstream filein;
    filein.open(filename.as_raw());

    if (filein.is_open()) {
        std::string contents;

        // stole this code from https://stackoverflow.com/questions/22984956/tellg-function-give-wrong-size-of-file/22986486#22986486
        filein.ignore(std::numeric_limits<std::streamsize>::max());
        std::streamsize length = filein.gcount();
        contents.resize(length);
        filein.clear();
        filein.seekg(0, std::ios_base::beg);

        filein.read(&contents[0], contents.size());

        filein.close();

        return std::make_unique<File>(File {filename.as_raw(), contents});
    } else {
        std::perror("Could not open file");
        return nullptr;
    }
}

// open a file {{{1
// llvm::raw_fd_ostream has no move assignment operator, no move constructor
#define OPENFILE(p, extrepl) \
    std::filesystem::path pathrepl (p.as_raw()); \
    pathrepl.replace_extension(extrepl); \
    std::string passtr (pathrepl.string()); \
    std::error_code ec; \
    llvm::raw_fd_ostream os (passtr, ec); \
    if (ec) \
        llvm::errs() << "Could not open output file \"" << passtr << "\": " << ec.message() << "\n";

// compile a file {{{1
int compile_file(OutFormats ofmt, NNPtr<char> filename) {
    auto source (read_file(filename));
    if (!source)
        return false;

    auto lexer = std::make_unique<Lexer>(*source);
    if (ofmt == OutFormats::LEX) {
        OPENFILE(filename, ".lexed.txt");
        if (os.has_error())
            return false;

        while (true) {
            Located<TokenData> t (lexer->next_token());
            if (Tokens::is<Tokens::_EOF>(t.value))
                break;

            os << format("{}: ({}) '{}'", t.span.as_rowcol(), Tokens::stringify_type(t.value), t.span.stringify());
        }

        os.close();
        return true;
    }

    auto parser = std::make_unique<Parser>(*lexer, *source);
    std::unique_ptr<ASTNS::CUB> parsed = parser->parse();

    if (!parsed)
        return false;

    if (ofmt == OutFormats::PARSE) {
        OPENFILE(filename, ".parsed.txt");
        if (os.has_error())
            return false;

        auto printv = std::make_unique<ASTNS::PrintVisitor>(os);
        parsed->accept(*printv); // dereference to use constructor NNPtr(T &other)

        os.close();
        return true;
    }

    auto m_unit = Codegen::codegen(*parsed);
    if (!m_unit.has())
        return false;

    auto unit (std::move(m_unit.get()));

    if (ofmt == OutFormats::CODEGEN) {
        OPENFILE(filename, ".kslir");
        if (os.has_error())
            return false;

        unit->print(os);

        os.close();
        return true;
    }

    /*
    auto lowerer = std::make_unique<Lower::Lowerer>(*unit);
    lowerer->lower();
    if (lowerer->errored)
        return false;

    if (ofmt == OutFormats::LOWER) {
        OPENFILE(filename, ".ll");
        if (os.has_error())
            return false;

        lowerer->print_mod(os);

        os.close();
        return true;
    }

    if (ofmt == OutFormats::OBJECT) {
        OPENFILE(filename, ".o");
        if (os.has_error())
            return false;

        if (!lowerer->objectify(os)) {
            os.close();
            return false;
        }

        os.close();
        return true;
    }
    */

    return true;
}

// main {{{1
int main(int argc, char *argv[]) {
    int opt;
    OutFormats ofmt = OutFormats::OBJECT;
    while ((opt = getopt(argc, argv, "f:e:")) != -1) {
        switch (opt) {
            case 'f':
#define OFMT(k, ku) \
    if (strcmp(optarg, #k) == 0) \
        ofmt = OutFormats::ku;
#define EOFMT(k, ku) else OFMT(k, ku)
                OFMT(lex, LEX)
                EOFMT(parse, PARSE)
                EOFMT(codegen, CODEGEN)
                EOFMT(lower, LOWER)
                EOFMT(object, OBJECT)
#undef OFMT
#undef EOFMT
                else {
                    std::cerr << "Invalid argument for option -f: '" << optarg << "'\n";
                    std::cerr << "Defaulting to -fobject\n";
                    ofmt = OutFormats::OBJECT;
                }
                break;

            case 'e':
                if (strcmp(optarg, "json") == 0)
                    errformat = ErrorFormat::JSON;
                else if (strcmp(optarg, "human") == 0)
                    errformat = ErrorFormat::HUMAN;
                else if (strcmp(optarg, "aligned") == 0)
                    errformat = ErrorFormat::ALIGNED;
                else {
                    std::cerr << "Invalid argument for option -e: '" << optarg << "'\n";
                    std::cerr << "Defaulting to -ehuman\n";
                    errformat = ErrorFormat::HUMAN;
                }
                break;

            default:
                break;
        }
    }

    if (optind >= argc) {
        std::cerr << "No input files\n";
        return 1;
    }

    enable_terminal_codes();

    bool success = true;
    for (; optind < argc; ++optind) {
        if (!compile_file(ofmt, NNPtr(*argv[optind])))
            success = false;
    }

    reset_terminal();
    return success ? 0 : 1;
}
