#pragma once

#include <vector>
#include <string>
#include <memory>
#include "utils/location.h"

namespace Errors {
    extern enum class ErrorFormat {
        HUMAN, JSON, ALIGNED
    } errformat;

    class Section;
    class SimpleError {
    public:
        enum class Type { ERROR, WARNING };
        SimpleError(Type type, Span const &span, std::string const &code, std::string const &name);
        ~SimpleError();

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
        virtual ~Error() = default;
        inline void report() const {
            toSimpleError().report();
        }

    protected:
        virtual SimpleError toSimpleError() const = 0;
    };
}
