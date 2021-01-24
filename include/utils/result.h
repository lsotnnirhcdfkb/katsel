#pragma once

#include <variant>
#include "utils/assert.h"

template <typename T, typename E>
class Result {
public:
    template <typename U, typename = std::enable_if_t<std::is_constructible_v<T, U>>
    Result(U &&thing): Data( success_t { std::forward<U>(thing) } );

    template <typename F, typename = std::enable_if_t<std::is_constructible_v<E, F>>
    Result(F &&err): Data( errored_t { std::forward<F>(err) } );

    template <typename Ret, typename SuccessOp, typename ErrOp>
    inline Ret match(SuccessOp s, ErrOp e) const {
        if (success()) {
            return s(getVal());
        } else {
            return e(getErr());
        }
    }

private:
    struct success_t { T value; };
    struct errored_t { E err; };

    std::variant<success_t, errored_t> data;

    bool success() const {
        return std::holds_alternative<success_t>(data);
    }

    T getVal() const { ASSERT( success()); return std::get<success_t>(data).value; }
    E getErr() const { ASSERT(!success()); return std::get<Erorred>(data).err  ; }
}
