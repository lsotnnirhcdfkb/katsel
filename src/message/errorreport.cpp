#include "message/errors.h"
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
/// report {{{1
void Error::report() const
{
    if (errformat == Format::HUMAN)
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

        struct showline
        {
            const File *file;
            int line;
        };
        std::vector<showline> showlines;

        for (Span const &span : spans)
            for (int i = getLineN(span.file.source.begin(), span.start); i < getLineN(span.file.source.begin(), span.end); ++i)
                showlines.push_back(showline {&span.file, i});

        for (Error::Underline const &u : underlines)
        {
            std::string::const_iterator begin = u.location.file->source.begin();
            for (int i = getLineN(begin, u.location.start); i <= getLineN(begin, u.location.end - 1); ++i)
                showlines.push_back(showline {u.location.file, i});
        }

        std::sort(showlines.begin(), showlines.end(), [](showline const &a, showline const &b) {
                return a.line < b.line;
            });
        std::stable_sort(showlines.begin(), showlines.end(), [](showline const &a, showline const &b) {
                return a.file->filename < b.file->filename;
            });

        int maxlinepad = 0;
        // i + 1 < instead of i < size - 1 because - 1 can overflow to the highest value and become true
        for (size_t i = 0; i + 1 < showlines.size(); )
        {
            if (showlines[i].file == showlines[i + 1].file && showlines[i].line == showlines[i + 1].line)
                showlines.erase(showlines.begin() + i + 1);
            else
                ++i;
        }

        for (size_t i = 0; i + 1 < showlines.size(); ++i)
            if (showlines[i].file == showlines[i + 1].file && showlines[i + 1].line - showlines[i].line > 1 && showlines[i + 1].line - showlines[i].line <= 3)
                for (int j = showlines[i].line + 1; j < showlines[i + 1].line; ++j)
                    showlines.insert(showlines.begin() + i + 1, showline {showlines[i].file, j});

        for (showline const &s : showlines)
        {
            int linew = 1, linenr = s.line;
            while (linenr /= 10)
                ++linew;
            maxlinepad = std::max(linew, maxlinepad);
        }

        std::string pad (maxlinepad + 1, ' ');
        File const *lastfile = nullptr;
        int lastnr = -1;
        for (showline const &sl : showlines)
        {
            if (sl.file != lastfile)
            {
                std::cerr << pad << "> " << attr(A_FG_CYAN, sl.file->filename) << std::endl;
                lastnr = -1;
            }

            {
                std::ios origState (nullptr);
                origState.copyfmt(std::cerr);

                if (sl.line != lastnr + 1 && lastnr != -1)
                    std::cerr << std::setw(maxlinepad) << std::right << std::string(maxlinepad, '.') << " | ...\n";

                std::cerr << std::setw(maxlinepad) << std::right << sl.line;
                std::cerr.copyfmt(origState);
            }

            std::cerr << " | ";

            std::string::const_iterator lstart, lend;
            getLine(lstart, lend, *sl.file, sl.line);

            std::vector<Underline const *> lchars;
            lchars.reserve(std::distance(lstart, lend));

            struct MessageColumn
            {
                Underline::Message const *message;
                int column;
            };
            std::vector<MessageColumn> linemessages;

            auto itInLoc = [](std::string::const_iterator const &i, Location const &l)
            {
                if (l.start == l.end)
                    return i == l.start;
                return i >= l.start && i < l.end;
            };

            bool needsecond = false;
            for (std::string::const_iterator i = lstart; i <= lend; ++i)
            {
                for (auto under = underlines.rbegin(); under != underlines.rend(); ++under) // do in reverse so that the first underlines are first when the messages are printed in reverse
                    if (itInLoc(i, under->location))
                    {
                        if ((under->location.end - 1 == i) || (under->location.start == under->location.end && under->location.start == i))
                        {
                            int underlinecoln = getColN(under->location.file->source.begin(), under->location.end - 1);
                            for (Underline::Message const &message : under->messages)
                                linemessages.push_back(MessageColumn {&message, underlinecoln});
                        }

                        needsecond = true;
                    }

                Underline const *charu = nullptr;
                for (Underline const &u : underlines)
                    if (itInLoc(i, u.location))
                    {
                        charu = &u;
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

            if (needsecond)
            {
                std::cerr << pad << "| ";
                for (Underline const *&i : lchars)
                {
                    if (i && i->messages.size()) // in a underline
                        std::cerr << attr(A_BOLD, attr(i->messages[0].color, std::string(1, i->ch)));
                    else if (i)
                        std::cerr << attr(A_BOLD, std::string(1, i->ch));
                    else
                        std::cerr << " ";
                }
                std::cerr << std::endl;
            }

            if (linemessages.size())
            {
                for (auto mescol = linemessages.rbegin(); mescol != linemessages.rend(); ++mescol)
                {
                    std::cerr << pad << "| ";
                    for (auto j = linemessages.begin(); j <= mescol.base() - 1; ++j)
                    {
                        j - linemessages.begin();
                        int lastcolumn = j == linemessages.begin() ? 0 : (j - 1)->column;
                        int diff = j->column - lastcolumn;

                        if (!diff)
                            continue;

                        int pamt = diff - 1;

                        std::cerr << std::string(pamt, ' ');
                        if (j->column < (mescol.base() - 1)->column)
                            std::cerr << '|';
                    }

                    bool lastmessage = mescol == linemessages.rend() - 1 || mescol->column != (mescol + 1)->column;
                    if (lastmessage)
                        std::cerr << attr(mescol->message->color, "`", true);
                    else
                        std::cerr << attr(mescol->message->color, "|", true);

                    std::cerr << "-- " << mescol->message->type << resetIfNecessary() << ": " << mescol->message->text << std::endl;
                }
            }

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
