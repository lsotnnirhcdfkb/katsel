#pragma once

#include <cstddef>
#include <cstdint>
#include "utils/assert.h"

template <typename T>
class NNPtr {
public:
    NNPtr(std::nullptr_t) = delete;
    NNPtr() = delete;
    NNPtr(T &other): ptr(&other) {}

    template <typename U, typename = std::enable_if_t<std::is_constructible_v<T*, U*>, void>>
    NNPtr(U *other): ptr(other) { ASSERT(ptr) }

    template <typename U, typename = std::enable_if_t<std::is_constructible_v<T*, U*>, void>>
    NNPtr(NNPtr<U> const &other): ptr(other.as_raw()) {}

    T& operator*()  const { return *ptr; }
    T* operator->() const { return  ptr; }
    T* as_raw()      const { return  ptr; }

    template <typename U>
    bool operator==(NNPtr<U> const &other) const { return ptr == other.ptr; }

    template <typename U>
    bool operator!=(NNPtr<U> const &other) const { return ptr != other.ptr; }

    friend struct std::hash<NNPtr<T>>;
private:
    T *ptr;
};

template <typename T> struct std::hash<NNPtr<T>> {
    size_t operator()(NNPtr<T> const &ptr) const {
        return reinterpret_cast<std::uintptr_t>(static_cast<void const*>(ptr.ptr));
    }
};
