#include "message/error.h"
#include "errors.h"
#include "message/ansistuff.h"
#include "utils/format.h"
#include <algorithm>
#include <iostream>
#include <iomanip>

// getLine {{{1
static void getLine(std::string::const_iterator &lstarto, std::string::const_iterator &lendo, File const &f, int linenr)
{
    int cline = linenr;
    std::string::const_iterator lstart = f.source.begin();
    for (; lstart < f.source.end() && cline > 1; ++lstart)
        if (*lstart == '\n')
            --cline;

    if (lstart == f.source.end())
    {
        lstarto = lendo = lstart;
        return;
    }

    auto lend (lstart);
    while (*lend != '\n' && lend != f.source.end())
        ++lend;

    lstarto = lstart;
    lendo = lend;
}
// getColN {{{1
static int getColN(std::string::const_iterator const &start, std::string::const_iterator loc)
{
    int coln = 1;

    for (; loc != start && *(loc - 1) != '\n'; ++coln, --loc);

    return coln;
}
// getLineN {{{1
static int getLineN(std::string::const_iterator const &start, std::string::const_iterator loc)
{
    int linen = 1; // current line will not have a newline to pass, but it still is a line
    while (loc > start)
    {
        if (*(loc - 1) == '\n') ++linen;
        --loc;
    }
    return linen;
}
// attr {{{1
inline static std::string attr(std::string const &ansicode, std::string const &message, bool noreset=false)
{
    if (ansiCodesEnabled())
    {
        if (noreset)
            return ansicode + message;
        else
            return ansicode + message + A_RESET;
    }
    else
        return message;
}
inline static char const * resetIfNecessary()
{
    if (ansiCodesEnabled())
        return A_RESET;
    else
        return "";
}
// helpers {{{1
// printHeading {{{2
void Error::printHeading() const
{
    std::string msgtypestr;
    switch (type)
    {
        case Error::MsgType::ERROR:
            msgtypestr = attr(A_BOLD A_FG_RED, "Error");
            break;
        case Error::MsgType::WARNING:
            msgtypestr = attr(A_BOLD A_FG_MAGENTA, "Warning");
            break;
    }
    std::string::const_iterator const fstart = location.file->source.cbegin();
    std::cerr << format("% at %:%:%:% %\n", msgtypestr, attr(A_FG_CYAN, location.file->filename, true), getLineN(fstart, location.start), getColN(fstart, location.start), resetIfNecessary(), message);
}
// collectShowlines {{{2
std::vector<Error::showline> Error::collectShowlines() const
{
    std::vector<showline> showlines;
    for (Error::Underline const &u : underlines)
    {
        std::string::const_iterator begin = u.location.file->source.begin();
        int startLineN = getLineN(begin, u.location.start), endLineN = getLineN(begin, u.location.end - 1);
        // because end is inclusive
        if ((endLineN + 1) - startLineN < 4)
            for (int i = startLineN; i <= endLineN; ++i)
                showlines.push_back(showline {u.location.file, i});
        else
        {
            showlines.push_back(showline {u.location.file, startLineN});
            showlines.push_back(showline {u.location.file, startLineN + 1});
            showlines.push_back(showline {u.location.file, endLineN});
            showlines.push_back(showline {u.location.file, endLineN - 1});
        }
    }

    std::sort(showlines.begin(), showlines.end(), [](showline const &a, showline const &b) {
            return a.line < b.line;
        });
    std::stable_sort(showlines.begin(), showlines.end(), [](showline const &a, showline const &b) {
            return a.file->filename < b.file->filename;
        });

    // i + 1 < instead of i < size - 1 because - 1 can overflow to the highest value and become true
    for (size_t i = 0; i + 1 < showlines.size(); )
        if (showlines[i].file == showlines[i + 1].file && showlines[i].line == showlines[i + 1].line)
            showlines.erase(showlines.begin() + i + 1);
        else
            ++i;

    for (size_t i = 0; i + 1 < showlines.size(); ++i)
        if (showlines[i].file == showlines[i + 1].file && showlines[i + 1].line - showlines[i].line > 1 && showlines[i + 1].line - showlines[i].line < 3)
            for (int j = showlines[i].line + 1; j < showlines[i + 1].line; ++j)
                showlines.insert(showlines.begin() + i + 1, showline {showlines[i].file, j});

    return showlines;
}
// countLinePad {{{2
int Error::countLinePad(std::vector<showline> const &showlines) const
{
    int maxlinepad = 0;
    for (showline const &s : showlines)
    {
        int linew = 1, linenr = s.line;
        while (linenr /= 10)
            ++linew;
        maxlinepad = std::max(linew, maxlinepad);
    }
    return maxlinepad;
}
// print predefined lines {{{2
void Error::printFileLine(std::string const &pad, File const *file) const
{
    std::cerr << pad << "> " << attr(A_FG_CYAN, file->filename) << std::endl;
}
void Error::printElipsisLine(std::string const &pad) const
{
    std::cerr << std::string(pad.size() - 1, '.') << " | ...\n";
}
// print an line with its underlines {{{2
void Error::printLine(showline const &sl, std::string const &pad) const
{
    std::string::const_iterator lstart, lend;
    getLine(lstart, lend, *sl.file, sl.line);

    std::vector<Underline const *> lchars;
    lchars.reserve(std::distance(lstart, lend));

    std::vector<MessageLocation> lineMessages;

    auto itInLoc = [](std::string::const_iterator const &i, Location const &l)
    {
        if (l.start == l.end)
            return i == l.start;
        return i >= l.start && i < l.end;
    };

    bool lineHasUnder = false;
    for (std::string::const_iterator i = lstart; i <= lend; ++i)
    {
        Underline const *charu = nullptr;
        for (Underline const &u : underlines)
            if (itInLoc(i, u.location))
            {
                charu = &u;
                lineHasUnder = true;
                break;
            }

        lchars.push_back(charu);

        if (i == lend); // dont print newline
        else if (charu && charu->messages.size())
            std::cerr << attr(A_BOLD, attr(charu->messages[0].color, std::string(1, *i)));
        else if (charu)
            std::cerr << attr(A_BOLD, std::string(1, *i));
        else
            std::cerr << *i;
    }

    std::cerr << std::endl;

    int maxRow = 0;
    for (std::string::const_iterator i = lend; i >= lstart; --i)
        for (Underline const &u : underlines)
            if (i == u.location.end - 1 || (u.location.start == u.location.end && i == u.location.start))
            {
                int col = getColN(u.location.file->source.begin(), u.location.end - 1) + 1;
                for (Underline::Message const &message : u.messages)
                {
                    int messagerow = 0;
                    if (lineMessages.size() > 0)
                    {
                        int messageEndCol = col + message.text.size() + message.type.size() + 5; // 3 for '-- ' and ': '
                        auto nextMessage = lineMessages.rbegin();
                        while (nextMessage != lineMessages.rend() && nextMessage->col <= messageEndCol)
                        {
                            messagerow = std::max(messagerow, nextMessage->row + 1);
                            ++nextMessage;
                        }
                    }
                    maxRow = std::max(maxRow, messagerow);

                    lineMessages.push_back(Error::MessageLocation {message, messagerow, col});
                }
            }

    if (lineHasUnder)
    {
        std::cerr << pad << "| ";
        for (unsigned int col = 0; col <= lchars.size(); ++col)
        {
            bool foundMessage = false;
            for (MessageLocation const &message : lineMessages)
            {
                if (message.row == 0 && message.col == col + 1)
                {
                    std::cerr << attr(message.message.color, "-- ", true) << message.message.type << resetIfNecessary() << ": " << message.message.text;
                    col = message.col + message.message.text.size() + message.message.type.size() + 5 - 1 - 1; // -1 because col is zero-based, and also -1 because of the ++col at the top of the for loop
                    foundMessage = true;
                    break;
                }
            }

            if (foundMessage)
                continue;
            if (col == lchars.size())
                continue;

            Underline const *un = lchars[col];
            if (un && un->messages.size()) // in a underline
                std::cerr << attr(A_BOLD, attr(un->messages[0].color, std::string(1, un->ch)));
            else if (un)
                std::cerr << attr(A_BOLD, std::string(1, un->ch));
            else
                std::cerr << " ";
        }
        std::cerr << std::endl;
    }


    if (lineMessages.size())
    {
        for (int row = 1; row <= maxRow; ++row)
        {
            std::cerr << pad << "| ";
            int lastcol = 1;
            for (auto message = lineMessages.rbegin(); message != lineMessages.rend(); ++message)
            {
                if (message->row == row)
                {
                    std::cerr << std::string(message->col - lastcol, ' ');
                    std::cerr << attr(message->message.color, "-- ", true) << message->message.type << resetIfNecessary() << ": " << message->message.text;

                    lastcol = message->col + message->message.text.size() + message->message.type.size() + 5;
                }
            }
            std::cerr << "\n";
        }
    }
}
// report {{{1
void Error::report() const
{
    if (errformat == ErrorFormat::HUMAN)
    {
        printHeading();
        auto showlines (collectShowlines());
        int maxlinepad (countLinePad(showlines));
        std::string pad (maxlinepad + 1, ' ');

        File const *lastfile = nullptr;
        int lastnr = -1;
        for (showline const &sl : showlines)
        {
            if (sl.file != lastfile)
            {
                printFileLine(pad, sl.file);
                lastnr = -1;
            }

            {
                std::ios origState (nullptr);
                origState.copyfmt(std::cerr);

                if (sl.line != lastnr + 1 && lastnr != -1)
                    printElipsisLine(pad);

                std::cerr << std::setw(maxlinepad) << std::right << sl.line;
                std::cerr.copyfmt(origState);
                std::cerr << " | ";
            }

            printLine(sl, pad);

            lastfile = sl.file;
            lastnr = sl.line;
        }
    }
    else
    {
        std::cerr << "{\"type\":\"";
        switch (type)
        {
            case Error::MsgType::ERROR:
                std::cerr << "error";
                break;
            case Error::MsgType::WARNING:
                std::cerr << "warning";
                break;
        }

        auto formatLocation = [](File const &f, std::string::const_iterator const &loc, std::string::const_iterator const &fstart) -> std::string
        {
            return format("{\"file\": \"%\", \"line\": %, \"column\": %, \"index\": %}", f.filename, getLineN(fstart, loc), getColN(fstart, loc), std::distance(fstart, loc));
        };

        std::cerr << "\",";
        std::string::const_iterator const fstart = location.file->source.cbegin();
        std::cerr << "\"location\":" << formatLocation(*location.file, location.start, fstart) << ",\"message\":\"" << message << "\",";

        std::cerr << "\"underlines\":[";
        bool f = true;
        for (Underline const &u : underlines)
        {
            if (!f) std::cerr << ",";
            f = false;

            std::cerr << "{\"start\":" << formatLocation(*u.location.file, u.location.start, fstart) << ", \"end\": " << formatLocation(*u.location.file, u.location.end, fstart) << ",\"char\":\"" << u.ch << "\"," << "\"messages\": [";

            bool fm = true;
            for (Underline::Message const &m : u.messages)
            {
                if (!fm) std::cerr << ",";
                fm = false;
                std::cerr << "{\"type\":\"" << m.type << "\",\"text\":\"" << m.text << "\"}";
            }
            std::cerr << "]}";
        }
        std::cerr << "]";

        std::cerr << "}\n";
    }
}
