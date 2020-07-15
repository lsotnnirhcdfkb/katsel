#include "errors.h"

void reportError(int line, int column, std::string message) {
    std::cout << "Error: " << message << "at " << line << ":" << column << std::endl;
}
