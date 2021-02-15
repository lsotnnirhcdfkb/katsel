#include "message/errors.h"

#include "message/ansistuff.h"

#include "utils/format.h"
#include "utils/file.h"

#include <iostream>

/*
// helpers {{{1
namespace {
    // get col/line number {{{2
    int get_col_n(std::string::const_iterator const &start, std::string::const_iterator loc) {
        int coln = 1;

        for (; loc != start && *(loc - 1) != '\n'; ++coln, --loc);

        return coln;
    }
    int get_line_n(std::string::const_iterator const &start, std::string::const_iterator loc) {
        int linen = 1; // current line will not have a newline to pass, but it still is a line
        while (loc > start) {
            if (*(loc - 1) == '\n') ++linen;
            --loc;
        }
        return linen;
    }
    // heading {{{2
    std::string color_of_type(MsgType type) {
        switch (type) {
            case MsgType::ERROR:   return A_BOLD A_FG_RED;
            case MsgType::WARNING: return A_BOLD A_FG_MAGENTA;
        }
    }
    void print_heading(MsgType type, Span const &span) {
        std::string msgtypestr;
        std::string const msg_color = color_of_type(type);
        switch (type) {
            case MsgType::ERROR:
                msgtypestr = attr(msg_color, "Error");
                break;
            case MsgType::WARNING:
                msgtypestr = attr(msg_color, "Warning");
                break;
        }
        std::string::const_iterator const fstart = span.start.file->source.cbegin();
        std::cerr << format("{} at {}:{}:{}{}:\n", msgtypestr, attr(FILEPATH_COLOR, span.start.file->filename, true), get_line_n(fstart, span.start.iter), get_col_n(fstart, span.start.iter), reset_if_necessary().as_raw());
    }
    // final line {{{2
    void print_final_line(std::string const &pad, MsgType type, std::string const &code, std::string const &name) {
        std::cerr << format("{}==> [{}]: {}\n", pad, attr(A_BOLD, code), name);
    }
    // printing lines {{{2
    void print_file_line(std::string const &pad, File const &file) {
        std::cerr << pad << "> " << attr(FILEPATH_COLOR, file.filename) << std::endl;
    }
    void print_elipsis_line(std::string const &pad) {
        std::cerr << std::string(pad.size() - 1, '.') << " | ...\n";
    }
    // print line with underlines {{{3
    void print_line(showline const &sl, std::string const &pad, std::vector<Underline> const &underlines) {
        std::string::const_iterator lstart, lend;
        get_line(lstart, lend, *sl.file, sl.line);

        std::vector<Maybe<NNPtr<Underline const>>> lchars;
        lchars.reserve(std::distance(lstart, lend));

        std::vector<MessageLocation> line_messages;

        auto it_in_span = [](std::string::const_iterator const &i, Span const &span) {
            if (span.start.iter == span.end.iter)
                return i == span.start.iter;
            return i >= span.start.iter && i < span.end.iter;
        };

        bool line_has_under = false;
        for (std::string::const_iterator i = lstart; i <= lend; ++i) {
            Maybe<NNPtr<Underline const>> charu;
            for (Underline const &u : underlines)
                if (it_in_span(i, u.span)) {
                    charu = Maybe<NNPtr<Underline const>>(NNPtr(u));
                    line_has_under = true;
                    break;
                }

            lchars.push_back(charu);

            if (i == lend); // dont print newline
            else
                charu.match(
                    [&i](NNPtr<Underline const> const &un) {
                        if (un->messages.size())
                            std::cerr << attr(A_BOLD, attr(un->messages[0].color.as_raw(), std::string(1, *i)));
                        else
                            std::cerr << attr(A_BOLD, std::string(1, *i));
                    },
                    [&i] {
                        std::cerr << *i;
                    });
        }

        std::cerr << std::endl;

        int max_row = 0;
        for (std::string::const_iterator i = lend; i >= lstart; --i)
            for (Underline const &u : underlines)
                if (i == u.span.end.iter - 1 || (u.span.start.iter == u.span.end.iter && i == u.span.start.iter)) {
                    int lcol = get_col_n(u.span.file->source.begin(), u.span.start.iter);
                    int col = get_col_n(u.span.file->source.begin(), u.span.end.iter - 1) + 1; // end - 1 to get the character that this ends at, +1 to get the next column. end by itself could wrap around to the next line
                    for (Message const &message : u.messages) {
                        int messagerow = 0;
                        if (line_messages.size() > 0) {
                            int message_end_col = col + message.text.size() + message.type.size() + 5; // 3 for '-- ' and ': '
                            auto next_message = line_messages.rbegin();
                            while (next_message != line_messages.rend() && next_message->lcol <= message_end_col) {
                                messagerow = std::max(messagerow, next_message->row + 1);
                                ++next_message;
                            }
                        }
                        max_row = std::max(max_row, messagerow);

                        line_messages.push_back(MessageLocation {message, messagerow, col, lcol});
                    }
                }

        if (line_has_under) {
            std::cerr << pad << "| ";
            for (unsigned int col = 0; col <= lchars.size(); ++col) {
                bool found_message = false;
                for (MessageLocation const &message : line_messages) {
                    if (message.row == 0 && message.col == col + 1) {
                        std::cerr << attr(message.message.color.as_raw(), "-- ", true) << message.message.type << ": " << reset_if_necessary().as_raw() << message.message.text;
                        col = message.col + message.message.text.size() + message.message.type.size() + 5 - 1 - 1; // -1 because col is zero-based, and also -1 because of the ++col at the top of the for loop
                        found_message = true;
                        break;
                    }
                }

                if (found_message)
                    continue;
                if (col == lchars.size())
                    continue;

                Maybe<NNPtr<Underline const>> &un = lchars[col];
                un.match(
                    [](NNPtr<Underline const> const &un) {
                        if (un->messages.size())
                            std::cerr << attr(A_BOLD, attr(un->messages[0].color.as_raw(), std::string(1, un->ch)));
                        else
                            std::cerr << attr(A_BOLD, std::string(1, un->ch));
                    },
                    [] {
                        std::cerr << " ";
                    });
            }
            std::cerr << std::endl;
        }


        if (line_messages.size()) {
            for (int row = 1; row <= max_row; ++row) {
                std::cerr << pad << "| ";
                int lastcol = 1;
                for (auto message = line_messages.rbegin(); message != line_messages.rend(); ++message) {
                    if (message->row == row) {
                        std::cerr << std::string(message->col - lastcol - 1, ' ');
                        std::cerr << attr(message->message.color.as_raw(), "`-- ", true) << message->message.type << ": " << reset_if_necessary().as_raw() << message->message.text;

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
            print_line(sl, pad, underlines);

    } else if (errformat == ErrorFormat::ALIGNED) {
            int un_start_col = get_col_n(fstart, un.span.start.iter);
            int un_end_col = get_col_n(fstart, un.span.end.iter);

            for (auto i = lstart; i != lend; ++i)
                if (i >= un.span.start.iter && i < un.span.end.iter)
                    if (un.messages.size())
                        std::cerr << attr(A_BOLD, attr(un.messages[0].color.as_raw(), std::string(1, *i)));
                    else
                        std::cerr << attr(A_BOLD, std::string(1, *i));
                else
                    std::cerr << *i;
            std::cerr << std::endl;

            std::cerr << pad << "| ";
            if (un_end_col <= un_start_col)
                un_end_col = un_start_col + 1;

            std::cerr << std::string(un_start_col - 1, ' ') << attr(A_BOLD, attr(un.messages.size() ? un.messages[0].color.as_raw() : "", std::string(un_end_col - un_start_col, '^'))) << std::endl;
        }

*/
