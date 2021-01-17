#pragma once

#include <variant>
#include "utils/assert.h"

template <typename T>
class Maybe {
public:
    Maybe(): data(Not()) {}
    Maybe(T &thing): data( With { thing } ) {}
    Maybe(T &&thing): data(With { std::forward<T>(thing) }) {}

    template <typename WithOp, typename NoOp>
    inline void match(WithOp withop, NoOp noop) const {
        if (has()) {
            withop(get());
        } else {
            noop();
        }
    }

    template <typename WithOp>
    inline void with(WithOp withop) const {
        match(withop, []{});
    }

private:
    struct Not {};
    struct With { T thing; };

    std::variant<Not, With> data;

    bool has() const {
        return std::holds_alternative<With>(data);
    }

    T get() const {
        ASSERT(has());
        return std::get<With>(data).thing;
    }
};
