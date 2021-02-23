#pragma once

#include <variant>
#include <type_traits>
#include "utils/assert.h"

template <typename T>
class Maybe {
public:
    Maybe(): data(not_t()) {}

    template <typename U, typename = std::enable_if_t<std::is_convertible_v<U, T>>>
    Maybe(U &&thing): data(with_t { std::forward<U>(thing) }) {}

    template <typename U, typename = std::enable_if_t<std::is_convertible_v<U, T>>>
    Maybe(Maybe<U> &&thing): data(thing.has() ? static_cast<decltype(data)>(with_t { std::forward<U>(thing.get()) }) : static_cast<decltype(data)>(not_t {})) {}

    template <typename WithOp, typename NoOp, typename Ret = std::invoke_result_t<WithOp, T const &>>
    inline Ret match(WithOp withop, NoOp noop) const {
        if (has()) {
            return withop(get());
        } else {
            return noop();
        }
    }

    template <typename WithOp, typename Ret = std::invoke_result_t<WithOp, T const &>>
    inline Maybe<Ret> fmap(WithOp withop) const {
        if (has()) {
            return Maybe<Ret>(withop(get()));
        } else {
            return Maybe<Ret>();
        }
    }

    template <typename WithOp>
    inline void with(WithOp withop) const {
        match(withop, []{});
    }

    bool has() const {
        return std::holds_alternative<with_t>(data);
    }

    T const &get() const {
        ASSERT(has())
        return std::get<with_t>(data).thing;
    }

    T &get() {
        ASSERT(has())
        return std::get<with_t>(data).thing;
    }

private:
    struct not_t {};
    struct with_t { T thing; };

    std::variant<not_t, with_t> data;
};
