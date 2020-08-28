#include <iostream>
#include <fstream>
#include <memory>
#include <limits>

#include "file.h"
#include "lexer.h"
#include "parser.h"
#include "llvmgenvisitor.h"

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

void compileFile(File &sourcefile)
{
    auto lexer = std::make_unique<Lexer>(sourcefile);
    auto parser = std::make_unique<Parser>(*lexer, sourcefile);

    std::unique_ptr<ASTs::AST> parsed = parser->parse();
    auto printv = std::make_unique<PrintVisitor>();
    auto llvmv = std::make_unique<LLVMGenVisitor>(sourcefile);

    if (parsed)
    {
        // parsed->accept(&*printv);
        parsed->accept(&*llvmv);
    }

    // int returnCode = parse(source);

    // if (returnCode != 0)
    // {
    //     exit(returnCode);
    // }
}

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
