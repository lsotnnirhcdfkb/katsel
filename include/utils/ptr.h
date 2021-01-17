#pragma once

#include <cstddef>

template <typename T>
class NNPtr {
public:
    NNPtr(std::nullptr_t) = delete;
    NNPtr() = delete;
    NNPtr(T &other): ptr(&other) {}

    T& operator*()  { return *ptr; }
    T* operator->() { return  ptr; }
    T* asNullable() { return  ptr; }

private:
    T *ptr;
};

