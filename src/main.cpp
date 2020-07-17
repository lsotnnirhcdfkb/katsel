#include <iostream>
#include <fstream>

#include "lexer.h"

std::string readFile(char *filename) {
    std::ifstream filein;
    filein.open(filename);

    if (filein.is_open()) {
        std::string contents;

        filein.seekg(0, std::ios::end);
        contents.resize(filein.tellg());

        filein.seekg(0, std::ios::beg);
        filein.read(&contents[0], contents.size());

        filein.close();
    
        return contents;
    } else {
        std::cerr << "Could not open file" << std::endl;
        return nullptr;
    }

}

void printTokenType(TokenType type) {
    switch (type) {
        case OPAREN: std::cout << "OPAREN"; break;
        case CPAREN: std::cout << "CPAREN"; break;
        case COMMA: std::cout << "COMMA"; break;
        case PERIOD: std::cout << "PERIOD"; break;
        case SEMICOLON: std::cout << "SEMICOLON"; break;

        case PLUS: std::cout << "PLUS"; break;
        case MINUS: std::cout << "MINUS"; break;
        case MULT: std::cout << "MULT"; break;
        case DIV: std::cout << "DIV"; break;

        case DOUBLEPLUS: std::cout << "DOUBLEPLUS"; break;
        case DOUBLEMINUS: std::cout << "DOUBLEMINUS"; break;

        case PLUSEQUAL: std::cout << "PLUSEQUAL"; break;
        case MINUSEQUAL: std::cout << "MINUSEQUAL"; break;
        case MULTEQUAL: std::cout << "MULTEQUAL"; break;
        case DIVEQUAL: std::cout << "DIVEQUAL"; break;

        case NOTEQUAL: std::cout << "NOTEQUAL"; break;
        case EQUAL: std::cout << "EQUAL"; break;
        case DOUBLEEQUAL: std::cout << "DOUBLEEQUAL"; break;
        case GREATER: std::cout << "GREATER"; break;
        case GREATEREQUAL: std::cout << "GREATEREQUAL"; break;
        case LESS: std::cout << "LESS"; break;
        case LESSEQUAL: std::cout << "LESSEQUAL"; break;

        case NOT: std::cout << "NOT"; break;
        case AND: std::cout << "AND"; break;
        case OR: std::cout << "OR"; break;
        case BITNOT: std::cout << "BITNOT"; break;
        case BITAND: std::cout << "BITAND"; break;
        case BITOR: std::cout << "BITOR"; break;
        case BITXOR: std::cout << "BITXOR"; break;

        case IDENTIFIER: std::cout << "IDENTIFIER"; break;

        case CHAR: std::cout << "CHAR"; break;
        case STRING: std::cout << "STRING"; break;

        case DECINT: std::cout << "DECINT"; break;
        case OCTINT: std::cout << "OCTINT"; break;
        case BININT: std::cout << "BININT"; break;
        case HEXINT: std::cout << "HEXINT"; break;

        case FLOAT: std::cout << "FLOAT"; break;
        case TRUE: std::cout << "TRUE"; break;
        case FALSE: std::cout << "FALSE"; break;
        case NULL_: std::cout << "NULL_"; break;

        case PRINT: std::cout << "PRINT"; break;

        case VOID: std::cout << "VOID"; break;
        case NAMESPACE: std::cout << "NAMESPACE"; break;
        case CLASS: std::cout << "CLASS"; break;

        case RETURN: std::cout << "RETURN"; break;

        case THIS: std::cout << "THIS"; break;

        case WHILE: std::cout << "WHILE"; break;
        case FOR: std::cout << "FOR"; break;
        case IF: std::cout << "IF"; break;
        case ELSE: std::cout << "ELSE"; break;
        case SWITCH: std::cout << "SWITCH"; break;

        case EOF_: std::cout << "EOF_"; break;
        case ERROR: std::cout << "ERROR"; break;
    }
}

void compileFile(char *filename) {
    std::string source (readFile(filename));

    Lexer lexer (source);

    while (true) {
        Token currentToken = lexer.nextToken();

        if (currentToken.type == TokenType::ERROR) {
            std::cout << "(Error token \"" << currentToken.message << "\") ";
        }
        
        std::string location = std::to_string(currentToken.line) + ":" + std::to_string(currentToken.column) + " | ";
        location.insert(location.begin(), 10 - location.size(), ' ');
        std::cout << location;

        printTokenType(currentToken.type);
        std::cout << ": \"" << std::string(currentToken.start, currentToken.end) << "\"" << std::endl;

        if (currentToken.type == TokenType::EOF_) {
            break;
        }
    }

    // int returnCode = parse(source);

    // if (returnCode != 0) {
    //     exit(returnCode);
    // }
}

int main(int argc, char *argv[]) {
    if (argc == 2) {
        // Compile file
        compileFile(argv[1]);
    } else {
        std::cerr << "Usage: " << argv[0] << " <file>\n"
            "\n"
            "file - the main file to compile\n" << std::endl;
        return 1;
    }
    return 0;

}
