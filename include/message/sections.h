#pragma once

#include "message/errors.h"
#include "utils/location.h"
#include "utils/maybe.h"

namespace Errors {
    class Section {
    public:
        virtual ~Section() = default;
        virtual int left_pad() const = 0;
        virtual void report(int left_pad) const = 0;
    };

    namespace Sections {
        class Underlines : public Section {
        public:
            struct Message {
                Span location;
                char underline_char;
                std::string message;
                std::string_view color;
            };

            Underlines(std::vector<Message> const &messages);

            std::vector<Message> messages;

            int left_pad() const override;
            void report(int left_pad) const override;
        };

        class Note : public Section {
        public:
            Note(Maybe<std::string> kind, std::string message);

            Maybe<std::string> kind;
            std::string message;

            int left_pad() const override;
            void report(int left_pad) const override;
        };
    }
}
