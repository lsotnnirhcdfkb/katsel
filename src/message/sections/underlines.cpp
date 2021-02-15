#include "../sections.h"
#include "../utils.h"
#include "message/ansistuff.h"
#include "utils/maybe.h"
#include "utils/format.h"

#include <algorithm>
#include <iostream>

using Errors::Sections::Underlines;
using namespace Errors::Utils;

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

            std::cerr << right_pad(left_pad - 1, cur_line.line) << " | ";

            // print_line(sl, left_pad, underlines);

            last_line_shown = cur_line;
        }

    } else if (errformat == Errors::ErrorFormat::ALIGNED) {
        for (auto const &message : messages) {
            std::string empty_pad (right_pad(left_pad, ""));
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
