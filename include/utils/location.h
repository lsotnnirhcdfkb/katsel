#pragma once

#include <string>
#include <sstream>
#include "utils/ptr.h"
#include "utils/file.h"

namespace ASTNS { class AST; }

class Location {
public:
    std::string::const_iterator iter;
    int line, column;
    NNPtr<File const> file;

    Location(std::string::const_iterator iter, int line, int column, NNPtr<File const> file);

    inline std::string as_rowcol() const {
        std::stringstream ss;
        ss << file->filename << ":" << line << ":" << column;
        return ss.str();
    }
};

class Span {
public:
    Location start;
    Location end;
    NNPtr<File const> file;

    Span(Location const &start, Location const &end);

    Span(ASTNS::AST const &v);

    inline std::string stringify() const {
        return std::string(start.iter, end.iter);
    }

    inline std::string as_rowcol() const {
        std::stringstream ss;
        ss << file->filename << ":" << start.line << ":" << start.column << ":" << end.line << ":" << end.column;
        return ss.str();
    }
};


template <typename T>
struct Located {
    Span span;
    T value;

    template <typename U = T, typename = std::enable_if_t<std::is_convertible_v<U, T>>>
    Located(Span const &span, U &&value): span(span), value(std::forward<U>(value)) {}
    
    template <typename U = T, typename = std::enable_if_t<std::is_convertible_v<U, T>>>
    Located(Span const &span, Located<U> &&value): span(span), value(std::forward<U>(value.value)) {}

    template <typename U = T, typename V, typename = std::enable_if_t<std::is_convertible_v<U, T>>>
    Located(Located<V> const &span, U &&value): span(span.span), value(std::forward<U>(value)) {}
};
