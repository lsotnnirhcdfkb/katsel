#pragma once

#include <string>
#include <sstream>
#include <utility>

#include "message/report_abort.h"

namespace _fmt {
    struct _FmtAction {
        enum class Type { INSERT_OPEN_BRACE, INSERT_CLOSE_BRACE, INSERT_ARG, INSERT_NEXT_CHAR } type;
        int di;
    };

    _FmtAction _decide_action(std::string::const_iterator i, std::string::const_iterator end);
    void _format(std::stringstream &ss, std::string const &fstr, std::string::const_iterator start);

    template <typename First, typename ... Formats>
    void _format(std::stringstream &ss, std::string const &fstr, std::string::const_iterator start, First &&first, Formats && ...f) {
        for (auto i = start; i != fstr.cend(); ) {
            _FmtAction a (_decide_action(i, fstr.cend()));
            switch (a.type) {
                case _FmtAction::Type::INSERT_OPEN_BRACE:
                    ss << '{';
                    break;

                case _FmtAction::Type::INSERT_CLOSE_BRACE:
                    ss << '}';
                    break;

                case _FmtAction::Type::INSERT_NEXT_CHAR:
                    ss << *i;
                    break;

                case _FmtAction::Type::INSERT_ARG:
                    ss << std::forward<First>(first);
                    _format(ss, fstr, i + a.di, std::forward<Formats>(f)...);
                    return;
            }

            i += a.di;
        }

        report_abort_noh("unused interpolation arguments in _format call");
    }
}

template <typename ... Formats>
std::string format(std::string const &fstr, Formats && ...f) {
    std::stringstream ss;
    _fmt::_format(ss, fstr, fstr.cbegin(), std::forward<Formats>(f)...);
    return ss.str();
}
