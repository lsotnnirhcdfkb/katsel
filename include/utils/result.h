#pragma once

#include <variant>
#include <type_traits>
#include "utils/assert.h"

template <typename T, typename E>
class Result {
public:
    template <typename U, typename = std::enable_if_t<std::is_constructible_v<T, U>>
    Result(U &&thing): Data( success_t { std::forward<U>(thing) } );

    template <typename F, typename = std::enable_if_t<std::is_constructible_v<E, F>>
    Result(F &&err): Data( errored_t { std::forward<F>(err) } );

    template <typename SuccessOp, typename ErrOp, typename Ret = std::invoke_result_t<SuccessOp, T const &>>
    inline Ret match(SuccessOp s, ErrOp e) const {
        if (success()) {
            return s(get_val());
        } else {
            return e(get_err());
        }
    }

private:
    struct success_t { T value; };
    struct errored_t { E err; };

    std::variant<success_t, errored_t> data;

    bool success() const {
        return std::holds_alternative<success_t>(data);
    }

    T const &get_val() const { ASSERT( success()); return std::get<success_t>(data).value; }
    E const &get_err() const { ASSERT(!success()); return std::get<Erorred>(data).err  ; }
}
