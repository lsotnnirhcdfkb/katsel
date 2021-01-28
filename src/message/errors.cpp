#include "errors.h"
#include "message/error.h"
#include "message/ansistuff.h"
#include "utils/format.h"
#include "ast/ast.h"
#include <iostream>
#include <cstdlib>
#include <typeinfo>

ErrorFormat errformat = ErrorFormat::HUMAN;
// Error methods {{{1
Error::Error(MsgType type, Span const &span, std::string const &code, std::string const &name):
    type(type), span(span),
    code(code), name(name) {}
Error& Error::underline(Underline const &underline) {
    underlines.push_back(underline);
    return *this;
}
// Underline message methods {{{1
Underline::Underline(Span const &span, char ch): span(span), ch(ch) {}
Underline& Underline::error(std::string const &message) {
    return addmsg("error", *A_FG_RED, message);
}
Underline& Underline::warning(std::string const &message) {
    return addmsg("warning", *A_FG_MAGENTA, message);
}
Underline& Underline::note(std::string const &message) {
    return addmsg("note", *A_FG_GREEN, message);
}
Underline& Underline::help(std::string const &message) {
    return addmsg("help", *A_FG_CYAN, message);
}
Underline& Underline::hint(std::string const &message) {
    return addmsg("hint", *A_FG_YELLOW, message);
}
Underline& Underline::message(std::string const &type, std::string const &message) {
    return addmsg(type, *(A_FG_WHITE A_BOLD), message);
}
Underline& Underline::addmsg(std::string const &type, NNPtr<char const> const color, std::string const &message) {
    messages.push_back(Message {type, message, color});
    return *this;
}
// other internal errors {{{1
void report_abort_noh(std::string const &message) {
    std::cerr << "!!! Unrecoverable brokenness discovered in compiler !!!: " << message << std::endl;
    std::cerr << "!!! this is a bug - whether or not it has a bug report is unknown" << std::endl;
    std::cerr << "!!! bugs can be reported on the Katsel GitHub page: https://github.com/hpj2ltxry43b/katsel/issues" << std::endl;
    std::cerr << "!!! please search far and wide (on the GitHub page) before reporting a bug, so that there are no duplicate bug reports!" << std::endl;
    std::cerr << "Aborting" << std::endl;
    std::abort();
}
void invalid_tok(std::string const &name, Span const &span) {
    report_abort_noh(format("invalid token for {}: \"{}\"", name, span.stringify()));
}
void called_with_op_ty_nethis(std::string const &class_n, std::string const &fnn, std::string const &opname) {
    report_abort_noh(format("{}::{} called with {} type != this", class_n, fnn, opname));
}
void out_oswitch_ddefault_lab(std::string const &fnn, Location const &highlight) {
    report_abort_noh(format("{} went out of switch despite default label", fnn));
}
void f_called(std::string const &fnn) {
    report_abort_noh(format("{} called", fnn));
}
void out_oswitch_noh(std::string const &fnn) {
    report_abort_noh(format("{} went out of switch", fnn));
}
