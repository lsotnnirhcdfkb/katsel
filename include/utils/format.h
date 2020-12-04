#pragma once

#include <string>

static inline void _format(std::stringstream &ss, std::string const &fstr, std::string::const_iterator start)
{
    for (auto i = start; i != fstr.cend(); ++i)
        if (*i == '%')
            if (*(i + 1) == '%')
            {
                ss << '%';
                ++i;
            }
            else
                reportAbortNoh("_format called with % in format string but no more format arguments");
        else
            ss << *i;
}

template <typename First, typename ... Formats>
static inline void _format(std::stringstream &ss, std::string const &fstr, std::string::const_iterator start, First const &first, Formats const &... f)
{
    for (auto i = start; i != fstr.cend(); ++i)
        if (*i == '%')
            if (*(i + 1) == '%')
            {
                ss << '%';
                ++i;
            }
            else
            {
                ss << first;
                _format(ss, fstr, i + 1, f...);
                break;
            }
        else
            ss << *i;
}

template <typename ... Formats>
std::string format(std::string const &fstr, Formats const &... f)
{
    std::stringstream ss;
    _format(ss, fstr, fstr.cbegin(), f...);
    return ss.str();
}
