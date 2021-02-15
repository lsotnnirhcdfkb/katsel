#pragma once
#include "message/errors.h"
#include "utils/location.h"

namespace Errors {
    namespace Sections {
        class Underlines : public Section {
        public:
            struct Message {
                Span location;
                std::string message;
                std::string_view color;
            };

            std::vector<Message> messages;

            int left_pad() const override;
            void report(int left_pad) const override;
        };
    }

}
