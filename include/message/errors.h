#pragma once

#include <vector>
#include <string>
#include <memory>
#include "utils/location.h"

namespace Errors {
    extern enum class ErrorFormat {
        HUMAN, JSON, ALIGNED
    } errformat;

    class Section {
    public:
        virtual ~Section() = default;
        virtual void report() const = 0;
    };

    class SimpleError {
    public:
        enum class Type { ERROR, WARNING };
        SimpleError(Type type, Span const &span, std::string const &code, std::string const &name);

        SimpleError &section(std::unique_ptr<Section> section);

        void report() const;

    private:
        Type type;
        Span span;

        std::string const code;
        std::string const name;
        std::vector<std::unique_ptr<Section>> sections;
    };

    class Error {
    public:
        virtual ~Error();
        inline void report() const {
            toBaseError().report();
        }

    protected:
        virtual SimpleError toBaseError() const = 0;
    };
}
