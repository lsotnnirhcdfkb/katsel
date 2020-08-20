#include "errors.h"

std::string getLine(std::string const &source, std::string::iterator const &start)
{
    auto linestart (start);
    while (linestart != source.begin() && *linestart != '\n') --linestart; // until *linestart is \n
    // once *linestart is \n then go forward once
    ++linestart;

    auto lineend(start);
    while (lineend != source.end() && *lineend != '\n') ++lineend;
    // *lineend should be \n
    
    return std::string(linestart, lineend);
    
}

void reportError(Token const &t, std::string const &message, std::string const &source)
{
    std::cerr << "Error at " << t.line << ":" << t.column << " - \"" << std::string(t.start, t.end) << "\": " << message << std::endl;
    std::cerr << " " << t.line << " " << getLine(source, t.start) << std::endl;
}

void reportDebug(Token const &t, std::string const &message, std::string const &source)
{
    std::cerr << "Debug message at " << t.line << ":" << t.column << ": " << message << std::endl;
    std::cerr << " " << t.line << " " << getLine(source, t.start) << std::endl;
}
