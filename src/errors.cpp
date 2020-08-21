#include "errors.h"

std::string getLine(File const &sourcefile, std::string::iterator const &start)
{
    auto linestart (start);
    while (linestart != sourcefile.source.begin() && *linestart != '\n') --linestart; // until *linestart is \n
    // once *linestart is \n then go forward once

    // if linestart == sourcefile.source.begin(), then loop stopped
    // because iterator hit beginning of sourcefile.source, not
    // because it hit \n, so there is no need to consume the \n
    if (linestart != sourcefile.source.begin())
        ++linestart;

    auto lineend(start);
    while (lineend != sourcefile.source.end() && *lineend != '\n') ++lineend;
    // *lineend should be \n
    
    return std::string(linestart, lineend);
    
}

void reportError(Token const &t, std::string const &message, File const &sourcefile)
{
    std::cerr << "Error at " << t.line << ":" << t.column << " - \"" << std::string(t.start, t.end) << "\": " << message << std::endl;
    std::cerr << " " << t.line << " | " << getLine(sourcefile, t.start) << std::endl;
}

void reportDebug(Token const &t, std::string const &message, File const &sourcefile)
{
    std::cerr << "Debug message at " << t.line << ":" << t.column << ": " << message << std::endl;
    std::cerr << " " << t.line << " | " << getLine(sourcefile, t.start) << std::endl;
}
