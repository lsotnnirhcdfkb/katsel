#pragma once

#include <variant>
#include "utils/assert.h"

template <typename T, typename E>
class Result {
public:
    Result(T &thing):  Data(Success { thing }) {}
    Result(T &&thing): Data(Success { std::forward<T>(thing) }) {}

    Result(E &err):  Data(Errored { err }) {}
    Result(E &&err): Data(Errored { std::forward<E>(err) }) {}

    template <typename Ret, typename SuccessOp, typename ErrOp>
    inline Ret match(SuccessOp s, ErrOp e) const {
        if (success()) {
            return s(getVal());
        } else {
            return e(getErr());
        }
    }

private:
    struct Success { T value; };
    struct Errored { E err; };

    std::variant<Success, Errored> data;

    bool success() const {
        return std::holds_alternative<Success>(data);
    }

    T getVal() const { ASSERT( success()); return std::get<Success>(data).value; }
    E getErr() const { ASSERT(!success()); return std::get<Erorred>(data).err  ; }
}
