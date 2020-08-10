#include "errors.h"

void reportError(Token &t, const std::string &message)
{
    std::cerr << "Error at " << t.line << ":" << t.column << ": " << message << std::endl;
}

void reportDebug(Token &t, const std::string &message)
{
    std::cerr << "Debug message at " << t.line << ":" << t.column << ": " << message << std::endl;
}
