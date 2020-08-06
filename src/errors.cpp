#include "errors.h"

void reportError(std::string message)
{
    std::cerr << "ERROR: " << message << std::endl;
}

void reportDebug(std::string message)
{
    std::cerr << "DEBUG: " << message << std::endl;
}
