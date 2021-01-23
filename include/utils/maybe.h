#pragma once

#include <variant>
#include "utils/assert.h"

template <typename T>
class Maybe {
public:
    Maybe(): data(not_t()) {}
    Maybe(T &thing):  data( with_t { thing } ) {}
    Maybe(T &&thing): data( with_t { std::forward<T>(thing) }) {}

    template <typename U, typename = std::enable_if_t<std::is_constructible_v<T, U>>>
    Maybe(U &&thing): data( with_t { std::forward<U>(thing) } ) {}

    template <typename Ret = void, typename WithOp, typename NoOp>
    inline Ret match(WithOp withop, NoOp noop) const {
        if (has()) {
            return withop(get());
        } else {
            return noop();
        }
    }

    template <typename WithOp>
    inline void with(WithOp withop) const {
        match<void>(withop, []{});
    }

    bool has() const {
        return std::holds_alternative<with_t>(data);
    }

    T get() const {
        ASSERT(has());
        return std::get<with_t>(data).thing;
    }

private:
    struct not_t {};
    struct with_t { T thing; };

    std::variant<not_t, with_t> data;
};