/// @file errors.cpp
/// Error reporting and formatting code

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

/// Convert a Token's start and end to a Location
/// @param sourcefile The file that the location is in
/// @param t The token to convert to a start and an end
Location TokenToLoc(File const &sourcefile, Token const &t)
{
    return Location {t.start, t.end, sourcefile};
}

/// Return a location that has the token and the line that the token is on
/// @param sourcefile The file that the location is in
/// @param t The token to get the line of
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

/// General error formatting function
/// @param message The message
/// @param sourcefile The source file
/// @param showl The location to show
/// @param underlinel The locations to underline
/// @param stream The stream to print to
void report(std::string &&message, File const &sourcefile, Location showl, std::vector<Location> underlinel, std::ostream &stream, bool ansiCodes)
{
    stream << message;

    if (ansiCodes) stream << "\033[0;2m";
    stream << " | ";

    if (ansiCodes) stream << "\033[0m";
    stream << std::string(showl.start, showl.end) << std::endl;

    if (ansiCodes) stream << "\033[0;2m";
    stream << " | ";
    if (ansiCodes) stream << "\033[1;36;1m";

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
    if (ansiCodes)
        stream << "\033[0m";
    stream << std::endl;
}

/// Report an error at a token with a message
/// @param t The token to error at
/// @param message The error message to report
/// @param sourcefile The source file that this error is in
void reportError(Token const &t, std::string const &message, File const &sourcefile)
{
    std::stringstream ss;
    if (ansiCodesEnabled())
        ss << "\033[31;1mError\033[0m at \033[37m" << sourcefile.filename << ":" << t.line << ":" << t.column << "\033[0m: " << message << std::endl;
    else
        ss << "Error at " << sourcefile.filename << ":" << t.line << ":" << t.column << ": " << message << std::endl;

    report(ss.str(), sourcefile, getLine(sourcefile, t), {TokenToLoc(sourcefile, t)}, std::cerr, ansiCodesEnabled());
}
