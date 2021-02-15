#include "sections.h"
#include "utils.h"
#include <iostream>
#include "message/ansistuff.h"

using namespace Errors::Utils;

int Errors::Sections::Underlines::left_pad() const {
    int max_line_nr = 0;
    for (auto const &message : messages) {
        max_line_nr = std::max(message.location.end.line, max_line_nr);
    }
    return width_of_int(max_line_nr);
}

void Errors::Sections::Underlines::report(int left_pad) const {
    std::cerr << "underlines" << std::endl;
}
