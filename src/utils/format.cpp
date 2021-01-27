#include "utils/format.h"

_fmt::_FmtAction _fmt::_decide_action(std::string::const_iterator i, std::string::const_iterator end) {
    char next = (i + 1) == end ? '\0' : *(i + 1);
    switch (*i) {
        case '{':
            switch (next) {
                case '{': return _FmtAction { _FmtAction::Type::INSERT_OPEN_BRACE, 2 };
                case '}': return _FmtAction { _FmtAction::Type::INSERT_ARG, 2 };
                default : report_abort_noh("expected '}' in format string");
            }

        case '}':
            switch (next) {
                case '}': return _FmtAction { _FmtAction::Type::INSERT_CLOSE_BRACE, 2 };
                default: report_abort_noh("unexpected '}' in format string");
            }

        default:
            return _FmtAction { _FmtAction::Type::INSERT_NEXT_CHAR, 1 };
    }
}

void _fmt::_format(std::stringstream &ss, std::string const &fstr, std::string::const_iterator start) {
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
                report_abort_noh("no more arguments to interpolate in _format call");
        }

        i += a.di;
    }
}
