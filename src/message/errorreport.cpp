#include "message/error.h"
#include "errors.h"
#include "message/ansistuff.h"
#include "utils/format.h"
#include <algorithm>
#include <iostream>
#include <iomanip>

#define FILEPATH_COLOR A_BOLD A_FG_CYAN

// helpers {{{1
namespace {
    // structs {{{2
    struct showline {
        NNPtr<File const> file;
        int line;
    };
    struct MessageLocation {
        Message const &message;
        int row, col, lcol;
    };
    // attr {{{2
    inline std::string attr(std::string const &ansicode, std::string const &message, bool noreset=false) {
        if (ansiCodesEnabled()) {
            if (noreset)
                return ansicode + message;
            else
                return ansicode + message + A_RESET;
        } else
            return message;
    }
    inline NNPtr<char const> resetIfNecessary() {
        if (ansiCodesEnabled())
            return *A_RESET;
        else
            return *"";
    }
    // jsonfield {{{2
    template <typename T>
    std::string jsonfield(std::string const &name, T v) {
        std::stringstream ss;
        ss << "\"" << name << "\": " << v;
        return ss.str();
    }
    // get line {{{2
    void getLine(std::string::const_iterator &lstarto, std::string::const_iterator &lendo, File const &f, int linenr) {
        int cline = linenr;
        std::string::const_iterator lstart = f.source.begin();
        for (; lstart < f.source.end() && cline > 1; ++lstart)
            if (*lstart == '\n')
                --cline;

        if (lstart == f.source.end()) {
            lstarto = lendo = lstart;
            return;
        }

        auto lend (lstart);
        while (*lend != '\n' && lend != f.source.end())
            ++lend;

        lstarto = lstart;
        lendo = lend;
    }
    // get col/line number {{{2
    int getColN(std::string::const_iterator const &start, std::string::const_iterator loc) {
        int coln = 1;

        for (; loc != start && *(loc - 1) != '\n'; ++coln, --loc);

        return coln;
    }
    int getLineN(std::string::const_iterator const &start, std::string::const_iterator loc) {
        int linen = 1; // current line will not have a newline to pass, but it still is a line
        while (loc > start) {
            if (*(loc - 1) == '\n') ++linen;
            --loc;
        }
        return linen;
    }
    // heading {{{2
    std::string colorOfType(MsgType type) {
        switch (type) {
            case MsgType::ERROR:   return A_BOLD A_FG_RED;
            case MsgType::WARNING: return A_BOLD A_FG_MAGENTA;
        }
    }
    void printHeading(MsgType type, Location const &location) {
        std::string msgtypestr;
        std::string const msgColor = colorOfType(type);
        switch (type) {
            case MsgType::ERROR:
                msgtypestr = attr(msgColor, "Error");
                break;
            case MsgType::WARNING:
                msgtypestr = attr(msgColor, "Warning");
                break;
        }
        std::string::const_iterator const fstart = location.file->source.cbegin();
        std::cerr << format("% at %:%:%:%\n", msgtypestr, attr(FILEPATH_COLOR, location.file->filename, true), getLineN(fstart, location.start), getColN(fstart, location.start), resetIfNecessary().asRaw());
    }
    // final line {{{2
    void printFinalLine(std::string const &pad, MsgType type, std::string const &code, std::string const &name) {
        std::cerr << format("%==> [%]: %\n", pad, attr(A_BOLD, code), name);
    }
    // collectShowlines {{{2
    std::vector<showline> collectShowlines(std::vector<Underline> const &underlines) {
        std::vector<showline> showlines;
        for (Underline const &u : underlines) {
            std::string::const_iterator begin = u.location.file->source.begin();
            int startLineN = getLineN(begin, u.location.start), endLineN = getLineN(begin, u.location.end - 1);
            // because end is inclusive
            if ((endLineN + 1) - startLineN < 4)
                for (int i = startLineN; i <= endLineN; ++i)
                    showlines.push_back(showline {u.location.file, i});
            else {
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
    int countLinePad(std::vector<showline> const &showlines) {
        int maxlinepad = 0;
        for (showline const &s : showlines) {
            int linew = 1, linenr = s.line;
            while (linenr /= 10)
                ++linew;
            maxlinepad = std::max(linew, maxlinepad);
        }
        return maxlinepad;
    }
    // printing lines {{{2
    void printFileLine(std::string const &pad, NNPtr<File const> file) {
        std::cerr << pad << "> " << attr(FILEPATH_COLOR, file->filename) << std::endl;
    }
    void printElipsisLine(std::string const &pad) {
        std::cerr << std::string(pad.size() - 1, '.') << " | ...\n";
    }
    // print line with underlines {{{3
    void printLine(showline const &sl, std::string const &pad, std::vector<Underline> const &underlines) {
        std::string::const_iterator lstart, lend;
        getLine(lstart, lend, *sl.file, sl.line);

        std::vector<Maybe<NNPtr<Underline const>>> lchars;
        lchars.reserve(std::distance(lstart, lend));

        std::vector<MessageLocation> lineMessages;

        auto itInLoc = [](std::string::const_iterator const &i, Location const &l) {
            if (l.start == l.end)
                return i == l.start;
            return i >= l.start && i < l.end;
        };

        bool lineHasUnder = false;
        for (std::string::const_iterator i = lstart; i <= lend; ++i) {
            Maybe<NNPtr<Underline const>> charu;
            for (Underline const &u : underlines)
                if (itInLoc(i, u.location)) {
                    charu = Maybe(NNPtr(u));
                    lineHasUnder = true;
                    break;
                }

            lchars.push_back(charu);

            if (i == lend); // dont print newline
            else
                charu.match(
                    [&i](NNPtr<Underline const> const &un) {
                        if (un->messages.size())
                            std::cerr << attr(A_BOLD, attr(un->messages[0].color.asRaw(), std::string(1, *i)));
                        else
                            std::cerr << attr(A_BOLD, std::string(1, *i));
                    },
                    [&i] {
                        std::cerr << *i;
                    });
        }

        std::cerr << std::endl;

        int maxRow = 0;
        for (std::string::const_iterator i = lend; i >= lstart; --i)
            for (Underline const &u : underlines)
                if (i == u.location.end - 1 || (u.location.start == u.location.end && i == u.location.start)) {
                    int lcol = getColN(u.location.file->source.begin(), u.location.start);
                    int col = getColN(u.location.file->source.begin(), u.location.end - 1) + 1; // end - 1 to get the character that this ends at, +1 to get the next column. end by itself could wrap around to the next line
                    for (Message const &message : u.messages) {
                        int messagerow = 0;
                        if (lineMessages.size() > 0) {
                            int messageEndCol = col + message.text.size() + message.type.size() + 5; // 3 for '-- ' and ': '
                            auto nextMessage = lineMessages.rbegin();
                            while (nextMessage != lineMessages.rend() && nextMessage->lcol <= messageEndCol) {
                                messagerow = std::max(messagerow, nextMessage->row + 1);
                                ++nextMessage;
                            }
                        }
                        maxRow = std::max(maxRow, messagerow);

                        lineMessages.push_back(MessageLocation {message, messagerow, col, lcol});
                    }
                }

        if (lineHasUnder) {
            std::cerr << pad << "| ";
            for (unsigned int col = 0; col <= lchars.size(); ++col) {
                bool foundMessage = false;
                for (MessageLocation const &message : lineMessages) {
                    if (message.row == 0 && message.col == col + 1) {
                        std::cerr << attr(message.message.color.asRaw(), "-- ", true) << message.message.type << ": " << resetIfNecessary().asRaw() << message.message.text;
                        col = message.col + message.message.text.size() + message.message.type.size() + 5 - 1 - 1; // -1 because col is zero-based, and also -1 because of the ++col at the top of the for loop
                        foundMessage = true;
                        break;
                    }
                }

                if (foundMessage)
                    continue;
                if (col == lchars.size())
                    continue;

                Maybe<NNPtr<Underline const>> &un = lchars[col];
                un.match(
                    [](NNPtr<Underline const> const &un) {
                        if (un->messages.size())
                            std::cerr << attr(A_BOLD, attr(un->messages[0].color.asRaw(), std::string(1, un->ch)));
                        else
                            std::cerr << attr(A_BOLD, std::string(1, un->ch));
                    },
                    [] {
                        std::cerr << " ";
                    });
            }
            std::cerr << std::endl;
        }


        if (lineMessages.size()) {
            for (int row = 1; row <= maxRow; ++row) {
                std::cerr << pad << "| ";
                int lastcol = 1;
                for (auto message = lineMessages.rbegin(); message != lineMessages.rend(); ++message) {
                    if (message->row == row) {
                        std::cerr << std::string(message->col - lastcol - 1, ' ');
                        std::cerr << attr(message->message.color.asRaw(), "`-- ", true) << message->message.type << ": " << resetIfNecessary().asRaw() << message->message.text;

                        lastcol = message->col + message->message.text.size() + message->message.type.size() + 6;
                    }
                }
                std::cerr << "\n";
            }
        }
    }
    // }}}3
}
// }}}1
// report {{{1
void Error::report() const {
    if (errformat == ErrorFormat::HUMAN) {
        printHeading(type, location);
        auto showlines (collectShowlines(underlines));
        int maxlinepad (countLinePad(showlines));
        std::string pad (maxlinepad + 1, ' ');

        Maybe<NNPtr<File const>> lastfile;
        int lastnr = -1;
        for (showline const &sl : showlines) {
            bool needFileLine = lastfile.match<bool>([&sl] (NNPtr<File const> file) -> bool {
                    // if there is a last file, check that it is != to the current line's file
                    return file != sl.file;
                },
                [] () -> bool {
                    // if there is no last file, then print a file line
                    return true;
                });
            if (needFileLine) {
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

            printLine(sl, pad, underlines);

            lastfile = Maybe(NNPtr(*sl.file));
            lastnr = sl.line;
        }

        printFinalLine(pad, type, code, name);
    } else if (errformat == ErrorFormat::ALIGNED) {
        printHeading(type, location);
        auto showlines (collectShowlines(underlines));
        int maxlinepad (countLinePad(showlines));
        std::string pad (maxlinepad + 1, ' ');

        for (Underline const &un : underlines) {
            auto fstart = un.location.file->source.begin();
            int lineN = getLineN(fstart, un.location.start);
            int colN = getColN(fstart, un.location.start);
            std::cerr << format("%> %:%:% %\n", pad, attr(FILEPATH_COLOR, un.location.file->filename, true), lineN, colN, resetIfNecessary().asRaw());
            for (Message const &me : un.messages)
                std::cerr << format("%| [%] %\n", pad, attr(me.color.asRaw(), me.type), me.text);

            std::ios origState (nullptr);
            origState.copyfmt(std::cerr);
            std::cerr << std::setw(maxlinepad) << std::right << lineN;
            std::cerr.copyfmt(origState);
            std::cerr << " | ";

            std::string::const_iterator lstart, lend;
            getLine(lstart, lend, *un.location.file, lineN);

            int unStartCol = getColN(fstart, un.location.start);
            int unEndCol = getColN(fstart, un.location.end);

            for (auto i = lstart; i != lend; ++i)
                if (i >= un.location.start && i < un.location.end)
                    if (un.messages.size())
                        std::cerr << attr(A_BOLD, attr(un.messages[0].color.asRaw(), std::string(1, *i)));
                    else
                        std::cerr << attr(A_BOLD, std::string(1, *i));
                else
                    std::cerr << *i;
            std::cerr << std::endl;

            std::cerr << pad << "| ";
            if (unEndCol <= unStartCol)
                unEndCol = unStartCol + 1;

            std::cerr << std::string(unStartCol - 1, ' ') << attr(A_BOLD, attr(un.messages.size() ? un.messages[0].color.asRaw() : "", std::string(unEndCol - unStartCol, '^'))) << std::endl;
        }

        printFinalLine(pad, type, code, name);
    } else {
        std::cerr << "{\"type\":\"";
        switch (type) {
            case MsgType::ERROR:
                std::cerr << "error";
                break;
            case MsgType::WARNING:
                std::cerr << "warning";
                break;
        }

        auto formatLocation = [](File const &f, std::string::const_iterator const &loc, std::string::const_iterator const &fstart) -> std::string {
            return format("{%, %, %, %}",
                    jsonfield("file", f.filename),
                    jsonfield("line", getLineN(fstart, loc)),
                    jsonfield("column", getColN(fstart, loc)),
                    jsonfield("index", std::distance(fstart, loc)));
        };

        std::cerr << "\",";
        std::string::const_iterator const fstart = location.file->source.cbegin();
        std::cerr << jsonfield("location", formatLocation(*location.file, location.start, fstart));
        std::cerr << jsonfield("code", code) << jsonfield("name", name);

        std::cerr << "\"underlines\":[";
        bool f = true;
        for (Underline const &u : underlines) {
            if (!f) std::cerr << ",";
            f = false;

            std::cerr << "{\"start\":" << formatLocation(*u.location.file, u.location.start, fstart) << ", \"end\": " << formatLocation(*u.location.file, u.location.end, fstart) << ",\"char\":\"" << u.ch << "\"," << "\"messages\": [";

            bool fm = true;
            for (Message const &m : u.messages) {
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
