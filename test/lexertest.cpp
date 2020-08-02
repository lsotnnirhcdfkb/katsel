#include "lexer.h"
#include <iostream>
#include <memory>

int main()
{
    // {{{ source file and expected tokens
    std::string source = 
        "( )\n"
        "[ ]\n"
        "{ }\n"
        ", . ; ? :\n"
        "! +\n"
        "- * /\n"
        "%\n"
        "= > <\n"
        "~ & | ^\n"
        "\n"
        "++ -- >> <<\n"
        "&& || == ::\n"
        "\n"
        "+= -= *= /=\n"
        "!= >= <= %=\n"
        "<<= >>= &= |= ^=\n"
        "\n"
        "alfkjs\n"
        "fajwoei\n"
        "a_2\n"
        "a_\n"
        "eoifjawegh439hsdifjw\n"
        "\n"
        "c'o'\n"
        "c\"a\"\n"
        "\"ewoaigasdfklwejagsld\"\n"
        "'afojwiealfkjwef'\n"
        "\n"
        "100123487019235\n"
        "0o30412375634672345\n"
        "0b101000110101101010\n"
        "0x1840329abcdefbcaef\n"
        "\n"
        "934.31892743\n"
        "true\n"
        "false\n"
        "null\n"
        "\n"
        "print void\n"
        "namespace class\n"
        "enum return this while for if else switch case default\n"
        "break breakall breakto const continue\n"
        "inline\n"
        "volatile\n"
        "\n"
        "print_\n"
        "void_\n"
        "_namespace\n"
        "class__\n"
        "__enum\n"
        "returnne\n"
        "breaka\n"
        "breakalla\n"
        "breaktoe\n"
        "int\n"
        "float\n"
        "double\n"
        "bool\n";

    TokenType expected[] =
    {
        TokenType::OPARN,
        TokenType::CPARN,
        TokenType::OSQUB,
        TokenType::CSQUB,
        TokenType::OCURB,
        TokenType::CCURB,
        TokenType::COMMA,
        TokenType::PERIOD,
        TokenType::SEMICOLON,
        TokenType::QUESTION,
        TokenType::COLON,
        TokenType::BANG,
        TokenType::PLUS,
        TokenType::MINUS,
        TokenType::STAR,
        TokenType::SLASH,
        TokenType::PERCENT,
        TokenType::EQUAL,
        TokenType::GREATER,
        TokenType::LESS,
        TokenType::TILDE,
        TokenType::AMPER,
        TokenType::PIPE,
        TokenType::CARET,
        TokenType::DOUBLEPLUS,
        TokenType::DOUBLEMINUS,
        TokenType::DOUBLEGREATER,
        TokenType::DOUBLELESS,
        TokenType::DOUBLEAMPER,
        TokenType::DOUBLEPIPE,
        TokenType::DOUBLEEQUAL,
        TokenType::DOUBLECOLON,
        TokenType::PLUSEQUAL,
        TokenType::MINUSEQUAL,
        TokenType::STAREQUAL,
        TokenType::SLASHEQUAL,
        TokenType::BANGEQUAL,
        TokenType::GREATEREQUAL,
        TokenType::LESSEQUAL,
        TokenType::PERCENTEQUAL,
        TokenType::DOUBLELESSEQUAL,
        TokenType::DOUBLEGREATEREQUAL,
        TokenType::AMPEREQUAL,
        TokenType::PIPEEQUAL,
        TokenType::CARETEQUAL,
        TokenType::IDENTIFIER,
        TokenType::IDENTIFIER,
        TokenType::IDENTIFIER,
        TokenType::IDENTIFIER,
        TokenType::IDENTIFIER,
        TokenType::CHARLIT,
        TokenType::CHARLIT,
        TokenType::STRINGLIT,
        TokenType::STRINGLIT,
        TokenType::DECINTLIT,
        TokenType::OCTINTLIT,
        TokenType::BININTLIT,
        TokenType::HEXINTLIT,
        TokenType::FLOATLIT,
        TokenType::TRUELIT,
        TokenType::FALSELIT,
        TokenType::NULLLIT,
        TokenType::PRINT,
        TokenType::VOID,
        TokenType::NAMESPACE,
        TokenType::CLASS,
        TokenType::ENUM,
        TokenType::RETURN,
        TokenType::THIS,
        TokenType::WHILE,
        TokenType::FOR,
        TokenType::IF,
        TokenType::ELSE,
        TokenType::SWITCH,
        TokenType::CASE,
        TokenType::DEFAULT,
        TokenType::BREAK,
        TokenType::BREAKALL,
        TokenType::BREAKTO,
        TokenType::CONST,
        TokenType::CONTINUE,
        TokenType::INLINE,
        TokenType::VOLATILE,
        TokenType::IDENTIFIER,
        TokenType::IDENTIFIER,
        TokenType::IDENTIFIER,
        TokenType::IDENTIFIER,
        TokenType::IDENTIFIER,
        TokenType::IDENTIFIER,
        TokenType::IDENTIFIER,
        TokenType::IDENTIFIER,
        TokenType::IDENTIFIER,
        TokenType::INT,
        TokenType::FLOAT,
        TokenType::DOUBLE,
        TokenType::BOOL,
        TokenType::EOF_
    };
    // }}}
    
    size_t passCount = 0;
    size_t failCount = 0;
    auto l = std::make_unique<Lexer>(source);
    size_t i = 0;

    while (true)
    {
        Token token = l->nextToken();

        std::string location = std::to_string(token.line) + ":" + std::to_string(token.column) + " | ";
        location.insert(location.begin(), 10 - location.size(), ' ');
        std::cout << location;

        if (token.type == expected[i])
        {
            std::cout << "PASS: ";
            ++passCount;
        }
        else
        {
            std::cout << "FAIL: ";
            ++failCount;
        }
        std::cout << "(" << token.type << ", expected " << expected[i] <<  ") \"" << std::string(token.start, token.end) << "\"" << std::endl;

        if (token.type == TokenType::EOF_)
        {
            break;
        }
        
        ++i;
    }

    if (failCount == 0)
    {
        std::cout << std::endl << "test passed" << std::endl;
        return 0;
    }
    else
    {
        std::cout << std::endl << "test failed with " << failCount << " failed and " << passCount << " passed" << std::endl;
        return 1;
    }
}
