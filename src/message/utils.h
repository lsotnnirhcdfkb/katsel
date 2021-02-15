#pragma once

#include <string>
#include <sstream>
#include <iomanip>
#include <iostream>
#include "utils/maybe.h"

namespace Errors {
    namespace Utils {
        inline int width_of_int(int x) {
            int width = 1;
            while (x /= 10)
                ++width;
            return width;
        }

        template <typename T>
        inline std::string right_pad(int pad, T const &t) {
            std::stringstream ss;
            ss << std::setw(pad) << std::right << t;
            return ss.str();
        }

        inline void print_file_line(int pad, File const &file) {
            std::cerr << right_pad(pad, "") << "> " << file.filename << "\n";
        }

        inline void print_elipsis_line(int pad) {
            std::cerr << std::string(pad - 1, '.') << " | ...\n";
        }

        inline Maybe<std::string_view> get_line(std::string const &str, int line) {
            int cur_line = line;

            unsigned int line_start = 0;
            for (; line_start < str.size() && cur_line > 1; ++line_start)
                if (str[line_start] == '\n')
                    --cur_line;

            if (line_start == str.size()) {
                return Maybe<std::string_view>();
            }

            unsigned int line_len = 0;
            while (str[line_start + line_len] != '\n' && line_start + line_len < str.size())
                ++line_len;

            return Maybe<std::string_view>(std::string_view(str).substr(line_start, line_len));
        }

        inline void print_line(std::string const &str, int line, int pad) {
            std::cerr << right_pad(pad - 1, line) << " | ";
            auto l = get_line(str, line);
            if (l.has())
                std::cerr << l.get();
            std::cerr << std::endl;
        }
    }
}
