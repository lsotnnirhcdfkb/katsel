#include <iostream>
#include <fstream>
#include <memory>
#include <limits>

#include "lexer.h"
#include "parser.h"
#include "visitor.h"

std::string readFile(char *filename)
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
    
        return contents;
    }
    else
    {
        std::cerr << "Could not open file" << std::endl;
        return nullptr;
    }
}

void compileFile(std::string &source)
{
    auto lexer = std::make_unique<Lexer>(source);
    auto parser = std::make_unique<Parser>(*lexer, source);

    std::unique_ptr<AST> parsed = parser->parse();
    PrintVisitor v;

    if (parsed)
        parsed->accept(&v);

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
        auto source = std::make_unique<std::string>(readFile(argv[1]));
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
