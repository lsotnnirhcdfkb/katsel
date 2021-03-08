#include "../sections.h"
#include "../utils.h"
#include "message/ansistuff.h"
#include "utils/maybe.h"
#include "utils/format.h"

using Errors::Sections::Note;
using namespace Errors::Utils;

Note::Note(Maybe<std::string> kind, std::string message): kind(kind), message(message) {}

int Note::left_pad() const {
    return 0;
}
void Note::report(int left_pad) const {
    if (errformat == Errors::ErrorFormat::HUMAN || errformat == Errors::ErrorFormat::ALIGNED) {
        print_line_prefix(left_pad, "", '-');

        std::cerr << if_ansi(A_BOLD);

        if (kind.has())
            std::cerr << kind.get();
        else 
            std::cerr << "note";

        std::cerr << if_ansi(A_RESET) << ": " << message << std::endl;

    }
}
