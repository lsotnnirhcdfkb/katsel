#pragma once

#include <string>
#include <sstream>
#include <utility>

#include "message/reportAbort.h"

namespace __fmt {
    struct __FmtAction {
        enum class Type { INSERT_OPEN_BRACE, INSERT_CLOSE_BRACE, INSERT_ARG, INSERT_NEXT_CHAR } type;
        int di;
    };

    static inline __FmtAction __decideAction(std::string::const_iterator i, std::string::const_iterator end) {
        char next = (i + 1) == end ? '\0' : *(i + 1);
        switch (*i) {
            case '{':
                switch (next) {
                    case '{': return __FmtAction { __FmtAction::Type::INSERT_OPEN_BRACE, 2 };
                    case '}': return __FmtAction { __FmtAction::Type::INSERT_ARG, 2 };
                    default : reportAbortNoh("expected '}' in format string");
                }

            case '}':
                switch (next) {
                    case '}': return __FmtAction { __FmtAction::Type::INSERT_CLOSE_BRACE, 2 };
                    default: reportAbortNoh("unexpected '}' in format string");
                }

            default:
                return __FmtAction { __FmtAction::Type::INSERT_NEXT_CHAR, 1 };
        }
    }

    static inline void __format(std::stringstream &ss, std::string const &fstr, std::string::const_iterator start) {
        for (auto i = start; i != fstr.cend(); ) {
            __FmtAction a (__decideAction(i, fstr.cend()));
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
                    reportAbortNoh("no more arguments to interpolate in __format call");
            }

            i += a.di;
        }
    }
    template <typename First, typename ... Formats>
    static void __format(std::stringstream &ss, std::string const &fstr, std::string::const_iterator start, First &&first, Formats && ...f) {
        for (auto i = start; i != fstr.cend(); ) {
            __FmtAction a (__decideAction(i, fstr.cend()));
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

        reportAbortNoh("unused interpolation arguments in __format call");
    }
}

template <typename ... Formats>
std::string format(std::string const &fstr, Formats && ...f) {
    std::stringstream ss;
    __fmt::__format(ss, fstr, fstr.cbegin(), std::forward<Formats>(f)...);
    return ss.str();
}
