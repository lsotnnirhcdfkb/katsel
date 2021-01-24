#pragma once

#include <string>
#include <sstream>
#include <utility>

#include "message/report_abort.h"

namespace __fmt {
    struct __FmtAction {
        enum class Type { INSERT_OPEN_BRACE, INSERT_CLOSE_BRACE, INSERT_ARG, INSERT_NEXT_CHAR } type;
        int di;
    };

    __FmtAction __decide_action(std::string::const_iterator i, std::string::const_iterator end);
    void __format(std::stringstream &ss, std::string const &fstr, std::string::const_iterator start);

    template <typename First, typename ... Formats>
    void __format(std::stringstream &ss, std::string const &fstr, std::string::const_iterator start, First &&first, Formats && ...f) {
        for (auto i = start; i != fstr.cend(); ) {
            __FmtAction a (__decide_action(i, fstr.cend()));
            switch (a.type) {
                case __FmtAction::Type::INSERT_OPEN_BRACE:
                    ss << '{';
                    break;

                case __FmtAction::Type::INSERT_CLOSE_BRACE:
                    ss << '}';
                    break;

                case __FmtAction::Type::INSERT_NEXT_CHAR:
                    ss << *i;
                    break;

                case __FmtAction::Type::INSERT_ARG:
                    ss << std::forward<First>(first);
                    __format(ss, fstr, i + a.di, std::forward<Formats>(f)...);
                    return;
            }

            i += a.di;
        }

        report_abort_noh("unused interpolation arguments in __format call");
    }
}

template <typename ... Formats>
std::string format(std::string const &fstr, Formats && ...f) {
    std::stringstream ss;
    __fmt::__format(ss, fstr, fstr.cbegin(), std::forward<Formats>(f)...);
    return ss.str();
}
