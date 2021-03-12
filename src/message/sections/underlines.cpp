#include "message/sections.h"
#include "../utils.h"
#include "message/ansistuff.h"
#include "utils/maybe.h"
#include "utils/format.h"

#include <algorithm>
#include <iostream>

using Errors::Sections::Underlines;
using namespace Errors::Utils;

Underlines::Underlines(std::vector<Message> const &messages): messages(messages) {}

int Underlines::left_pad() const {
    int max_line_nr = 0;
    for (auto const &message : messages) {
        max_line_nr = std::max(message.location.end.line, max_line_nr);
    }
    return width_of_int(max_line_nr) + 1;
}

void Underlines::report(int left_pad) const {
    if (errformat == Errors::ErrorFormat::HUMAN) {
        struct ShowLine {
            int line;
            NNPtr<File const> file;

            bool operator==(ShowLine const &other) const {
                return line == other.line && file == other.file;
            }
        };

        std::vector<ShowLine> showlines;

        for (auto const &message : messages) {
            showlines.push_back(ShowLine { message.location.start.line, message.location.start.file });
            showlines.push_back(ShowLine { message.location.end.line, message.location.end.file });
        }

        std::sort(showlines.begin(), showlines.end(), [] (ShowLine const &a, ShowLine const &b) {
                    return a.line < b.line;
                });
        std::stable_sort(showlines.begin(), showlines.end(), [] (ShowLine const &a, ShowLine const &b) {
                    return a.file->filename < b.file->filename;
                });

        for (size_t i = 0; i + 1 < showlines.size(); )
            if (showlines[i] == showlines[i + 1])
                showlines.erase(showlines.begin() + i + 1);
            else
                ++i;

        Maybe<ShowLine> last_line_shown;

        for (auto const &cur_line : showlines) {
            last_line_shown.with(
                [&cur_line, &left_pad] (ShowLine last_line) {
                    if (last_line.file != cur_line.file) {
                        print_file_line(left_pad, *cur_line.file);
                    } else if (last_line.line + 1 != cur_line.line) {
                        print_elipsis_line(left_pad);
                    }
                }
            );

            print_line_prefix(left_pad, cur_line.line, '|');

            {
                std::string_view line (get_line(cur_line.file->source, cur_line.line).match(
                                [] (std::string_view const &sv) {
                                    return sv;
                                },
                                [] {
                                    return std::string_view("");
                                }));

                struct Underline {
                    std::string_view color;
                    char ch;
                };
                std::vector<Underline> char_underlines;

                for (std::string_view::const_iterator char_iter = line.cbegin(); char_iter <= line.cend(); ++char_iter) {
                    Maybe<NNPtr<Message const>> char_message;
                    for (Message const &message : messages) {
                        bool in_message = false;
                        if (message.location.start.iter == message.location.end.iter) {
                            in_message = char_iter == &*message.location.start.iter;
                        } else {
                            in_message = char_iter >= &*message.location.start.iter && char_iter < &*message.location.end.iter;
                        }

                        if (in_message) {
                            char_message = message;
                            break;
                        }
                    }


                    Underline current;
                    if (char_message.has()) {
                        current = { char_message.get()->color, char_message.get()->underline_char };
                    } else {
                        current = { A_RESET, ' ' };
                    }

                    char_underlines.push_back(current);
                }

                int col = 0;
                for (auto iter = line.cbegin(); iter != line.cend(); ++iter) {
                    auto const &u (char_underlines[col]);
                    std::cerr << if_ansi(u.color) << *iter << if_ansi(A_RESET);
                    ++col;
                }
                std::cerr << A_RESET "\n";

                struct MessageLocation {
                    NNPtr<Message const> message;
                    std::string text;
                    unsigned int row;
                    unsigned int start_col;

                    MessageLocation(Message const &message, int row):
                        message(message),
                        row(row) {
                        if (row == 0) {
                            start_col = get_col_n(message.location.file->source.begin(), message.location.end.iter - 1) + 1;
                            text = "-- " + message.message;
                        } else {
                            start_col = get_col_n(message.location.file->source.begin(), message.location.end.iter - 1);
                            text = "`-- " + message.message;
                        }
                    }

                    unsigned int end_col() const {
                        return start_col + text.size();
                    }
                };
                std::vector<MessageLocation> located_line_messages;
                std::vector<NNPtr<Message const>> line_messages;

                for (auto char_iter = line.cbegin(); char_iter <= line.cend(); ++char_iter)
                    for (Message const &message : messages)
                        if (message.location.start.iter == message.location.end.iter) {
                            if (char_iter == &*message.location.start.iter)
                                line_messages.push_back(message);
                        } else if (char_iter == &*(message.location.end.iter - 1)) {
                            line_messages.push_back(message);
                        }

                unsigned int max_message_row = 0;
                for (auto message = line_messages.crbegin(); message < line_messages.crend(); ++message) {
                    unsigned int message_row = 0;
                    while (true) {
                        auto as_loc = MessageLocation(**message, message_row);
                        bool overlapping = false;

                        for (auto const &m : located_line_messages)
                            if (m.row == message_row && as_loc.end_col() >= m.start_col) {
                                overlapping = true;
                                break;
                            }

                        if (!overlapping)
                            break;
                        else
                            ++message_row;
                    }

                    max_message_row = std::max(message_row, max_message_row);
                    located_line_messages.push_back(MessageLocation(**message, message_row));
                }

                // +2 because
                //     with +0 will only print before the newline at the end of the line (get_line doesn't return a trailing newline)
                //     with +1 will only print the underline under the 'newline'
                //     with +2 will print the underline under the 'newline', and the message to the right of it

                if (line_messages.size()) {
                    std::cerr << rjust(left_pad, "") << "| ";
                    for (unsigned int col = 1; col <= line.length() + 2;) {
                        bool printed_message = false;
                        for (auto const &located_message : located_line_messages) {
                            if (located_message.row == 0 && located_message.start_col == col) {
                                std::cerr << if_ansi(located_message.message->color) << located_message.text << if_ansi(A_RESET);
                                col = located_message.end_col();
                                printed_message = true;
                                break;
                            }
                        }

                        if (!printed_message) {
                            if ((col - 1) < char_underlines.size()) {
                                Underline const &u (char_underlines[col - 1]);
                                std::cerr << if_ansi(u.color) << u.ch << if_ansi(A_RESET);
                            }
                            ++col;
                        }
                    }
                    std::cerr << A_RESET "\n";

                    for (unsigned int row = 1; row <= max_message_row; ++row) {
                        print_line_prefix(left_pad, "", '|');
                        for (unsigned int col = 1; col <= line.length() + 2;) {
                            bool need_space = true;
                            for (auto const &located_message : located_line_messages) {
                                if (located_message.start_col == col) {
                                    if (located_message.row == row) {
                                        std::cerr << if_ansi(located_message.message->color) << located_message.text << if_ansi(A_RESET);
                                        col = located_message.end_col();
                                        need_space = false;
                                        break;
                                    } else if (located_message.row > row) {
                                        std::cerr << "|";
                                        ++col;
                                        need_space = false;
                                        break;
                                    }
                                }
                            }

                            if (need_space) {
                                std::cerr << " ";
                                ++col;
                            }
                        }
                        std::cerr << A_RESET "\n";
                    }
                }
            }

            last_line_shown = cur_line;
        }

    } else if (errformat == Errors::ErrorFormat::ALIGNED) {
        for (auto const &message : messages) {
            std::string empty_pad (rjust(left_pad, ""));
            std::cerr << empty_pad << format("> {}{}:{}:{}{}\n", if_ansi(A_FG_CYAN), message.location.file->filename, message.location.start.line, message.location.start.column, if_ansi(A_RESET));
            print_line(message.location.start.file->source, message.location.start.line, left_pad);

            int start_col = message.location.start.column;
            std::cerr << empty_pad << "| " << std::string(start_col - 1, ' ');

            if (message.location.end.line == message.location.start.line) {
                int end_col = message.location.end.column;
                std::cerr << if_ansi(message.color) << std::string(end_col - start_col, message.underline_char) << if_ansi(A_RESET) << " ";
            } else {
                std::cerr << if_ansi(message.color) << std::string(5, message.underline_char) << "..." << if_ansi(A_RESET) << " ";
            }

            std::cerr << message.message << std::endl;

        }
    }
}
