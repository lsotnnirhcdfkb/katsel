#include <iostream>
#include <fstream>
#include <memory>

#include "lexer.h"
#include "parser.h"

std::string readFile(char *filename)
{
    std::ifstream filein;
    filein.open(filename);

    if (filein.is_open())
    {
        std::string contents;

        filein.seekg(0, std::ios::end);
        contents.resize(filein.tellg());

        filein.seekg(0, std::ios::beg);
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

void compileFile(std::unique_ptr<std::string> &source)
{
    auto lexer = std::make_unique<Lexer>(*source);
    auto parser = std::make_unique<Parser>(*lexer);

    std::unique_ptr<AST> node = parser->parse();
    node->print();

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
        compileFile(source);
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
