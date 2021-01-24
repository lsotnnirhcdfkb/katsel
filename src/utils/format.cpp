#include "utils/format.h"

__fmt::__FmtAction __fmt::__decide_action(std::string::const_iterator i, std::string::const_iterator end) {
    char next = (i + 1) == end ? '\0' : *(i + 1);
    switch (*i) {
        case '{':
            switch (next) {
                case '{': return __FmtAction { __FmtAction::Type::INSERT_OPEN_BRACE, 2 };
                case '}': return __FmtAction { __FmtAction::Type::INSERT_ARG, 2 };
                default : report_abort_noh("expected '}' in format string");
            }

        case '}':
            switch (next) {
                case '}': return __FmtAction { __FmtAction::Type::INSERT_CLOSE_BRACE, 2 };
                default: report_abort_noh("unexpected '}' in format string");
            }

        default:
            return __FmtAction { __FmtAction::Type::INSERT_NEXT_CHAR, 1 };
    }
}

void __fmt::__format(std::stringstream &ss, std::string const &fstr, std::string::const_iterator start) {
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
                report_abort_noh("no more arguments to interpolate in __format call");
        }

        i += a.di;
    }
}
