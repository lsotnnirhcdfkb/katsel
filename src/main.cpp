/// @file main.cpp
/// Compiler entry point
/// Runs all the things that need to happen in order to compile, as well as parses arguments

#include <iostream>
#include <fstream>
#include <memory>
#include <limits>

#include "parse/ast.h"
#include "parse/parser.h"
#include "utils/file.h"
#include "lex/lexer.h"
#include "message/ansistuff.h"
#include "visit/printvisitor.h"

/// Read a file and output a File, with the appropriate source string
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

/// Compile a file to object code
void compileFile(File &sourcefile)
{
    enableTerminalCodes();
    auto lexer = std::make_unique<Lexer>(sourcefile);
    auto parser = std::make_unique<Parser>(*lexer, sourcefile);

    std::unique_ptr<ASTNS::Program> parsed = parser->parse();
    auto printv = std::make_unique<PrintVisitor>();

    if (parsed)
    {
        // compile(&*parsed, sourcefile);
        printv->visitProgram(parsed.get());
    }

    resetTerminal();

    // int returnCode = parse(source);

    // if (returnCode != 0)
    // {
    //     exit(returnCode);
    // }
}

/// Main entry point
int main(int argc, char *argv[])
{
    if (argc == 2)
    {
        // Compile file
        auto source = std::make_unique<File>(readFile(argv[1]));
        compileFile(*source);
    }
    else
    {
        std::cerr << "Usage: " << argv[0] << " <file>\n"
            "\n"
            "file - the main file to compile\n" << std::endl;
        return 1;
    }
    return 0;
}
