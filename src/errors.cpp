#include "errors.h"

/// A struct representing a location in a File
struct Location
{
    /// The start of this location
    std::string::iterator const start;
    /// The end of this location
    std::string::iterator const end;
    /// The file that this location is in
    File const &sourcefile;
};

Location TokenToLoc(File const &sourcefile, Token const &t)
{
    return Location {t.start, t.end, sourcefile};
}

Location getLine(File const &sourcefile, Token const &t)
{
    auto linestart (t.start);
    while (linestart != sourcefile.source.begin() && *linestart != '\n') --linestart; // until *linestart is \n
    // once *linestart is \n then go forward once

    // if linestart == sourcefile.source.begin(), then loop stopped
    // because iterator hit beginning of sourcefile.source, not
    // because it hit \n, so there is no need to consume the \n
    if (linestart != sourcefile.source.begin())
        ++linestart;

    auto lineend(t.end);
    while (lineend != sourcefile.source.end() && *lineend != '\n') ++lineend;
    // *lineend should be \n

    Location l {linestart, lineend, sourcefile};
    return l;
}

void report(std::string &&message, File const &sourcefile, Location showl, std::vector<Location> underlinel, std::ostream &stream)
{
    stream << message;
    stream << " | " << std::string(showl.start, showl.end) << std::endl;
    stream << " | ";

    std::string::iterator it = showl.start;
    while (true)
    {
        bool draw, arrow = draw = false;
        bool done = true;
        for (Location &l : underlinel)
        {
            if (it >= l.start && it < l.end)
            {
                draw = true;
                done = false;
                if (std::distance(l.start, l.end) == 1)
                    arrow = true;
            }

            if (it < l.start)
            {
                done = false;
            }
        }

        if (draw)
        {
            if (arrow)
                stream << "^";
            else
                stream << "~";
        }
        else
            stream << " ";

        if (done)
            break;

        ++it;
    }
    stream << std::endl;
}
void reportError(Token const &t, std::string const &message, File const &sourcefile)
{
    std::stringstream ss;
    ss << "Error at " << sourcefile.filename << ":" << t.line << ":" << t.column << ": " << message << std::endl;
    report(ss.str(), sourcefile, getLine(sourcefile, t), {TokenToLoc(sourcefile, t)}, std::cerr);
}
