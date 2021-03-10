#include "message/errors.h"
#include "message/sections.h"
#include "json.h"

#include "message/ansistuff.h"

#include "utils/format.h"

#include <iostream>
#include <cstdlib>

Errors::ErrorFormat Errors::errformat = Errors::ErrorFormat::HUMAN;

Errors::SimpleError::SimpleError(Type type, Span const &span, std::string const &code, std::string const &name):
    type(type), span(span),
    code(code), name(name) {}

Errors::SimpleError& Errors::SimpleError::section(std::unique_ptr<Section> section) {
    sections.push_back(std::move(section));
    return *this;
}

void Errors::SimpleError::report() const {
    if (errformat == ErrorFormat::HUMAN || errformat == ErrorFormat::ALIGNED) {
        std::string_view msg_type_str;
        std::string_view msg_type_color;
        switch (type) {
            case Type::ERROR:
                msg_type_str = "error";
                msg_type_color = A_BOLD A_FG_RED;
                break;
            case Type::WARNING:
                msg_type_str = "warning";
                msg_type_color = A_BOLD A_FG_MAGENTA;
                break;
        }
        std::cerr << format("{}{}{} at {}{}{}:\n", if_ansi(msg_type_color), msg_type_str, if_ansi(A_RESET), if_ansi(A_FG_CYAN A_BOLD), span.start.as_rowcol(), if_ansi(A_RESET));

        int left_pad = 0;
        for (auto const &section : sections)
            left_pad = std::max(section->left_pad(), left_pad);

        for (auto const &section : sections)
            section->report(left_pad);

        std::cerr << format("{}==> [{}{}{}]: {}\n", std::string(left_pad, ' '), if_ansi(A_BOLD), code, if_ansi(A_RESET), name);
    } else {
        auto format_location = [](Location const &l) -> JSON::Object {
            return JSON::Object {
                {
                    { "file", JSON::String { l.file->filename } },
                    { "line", JSON::Number { static_cast<double>(l.line) } },
                    { "column", JSON::Number { static_cast<double>(l.column) } },
                    { "index", JSON::Number { static_cast<double>(std::distance(l.file->source.cbegin(), l.iter)) } }
                }
            };
        };

        JSON::Object obj {
            {
                { "type", JSON::String { type == Type::ERROR ? "error" : "warning" } },
                { "start", format_location(span.start) },
                { "end", format_location(span.end) },
                { "code", JSON::String { code } },
                { "name", JSON::String { name } }
            }
        };

        std::cerr << obj.stringify() << std::endl;
    }
}

void report_abort_noh(std::string const &message) {
    std::cerr << "!!! katselc is broken !!!: " << message << std::endl;
    std::cerr << "Aborting" << std::endl;
    std::abort();
}
