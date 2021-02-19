#pragma once

#include <variant>
#include <type_traits>
#include "utils/assert.h"

template <typename T, typename E>
class Result {
public:
    template <typename U, typename = std::enable_if_t<std::is_convertible_v<U, T>>>
    static Result<T, E> Success(U &&thing) {
        return Result<T, E> { std::forward<U>(thing) };
    }

    template <typename F, typename = std::enable_if_t<std::is_convertible_v<F, E>>>
    static Result<T, E> Error(F &&thing) {
        return Result<T, E> { std::forward<F>(thing) };
    }

    template <typename DataType, typename ErrType,
             typename = std::enable_if_t<std::is_convertible_v<DataType, T>>,
             typename = std::enable_if_t<std::is_convertible_v<ErrType, E>>>
    Result(Result<DataType, ErrType> &&thing):
        data {
            thing.success() ?
            thing.get_val() :
            thing.get_err()
        } {}
                 

    template <typename SuccessOp, typename ErrOp, typename Ret = std::invoke_result_t<SuccessOp, T const &>>
    inline Ret match(SuccessOp s, ErrOp e) const {
        if (success()) {
            return s(result());
        } else {
            return e(error());
        }
    }

    bool success() const {
        return std::holds_alternative<success_t>(data);
    }

    T const &result() const { ASSERT( success()); return std::get<success_t>(data).value; }
    E const &error() const { ASSERT(!success()); return std::get<errored_t>(data).err  ; }

private:
    struct success_t { T value; };
    struct errored_t { E err; };

    std::variant<success_t, errored_t> data;
};
