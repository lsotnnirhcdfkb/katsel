#include "errors.h"
#include "message/error.h"
#include "message/ansistuff.h"
#include "utils/format.h"
#include <iostream>
#include <cstdlib>
#include <typeinfo>

ErrorFormat errformat = ErrorFormat::HUMAN;
// constructors for location {{{1
Location::Location(Token const &t): start(t.start), end(t.end), file(t.sourcefile) {}
Location::Location(IR::ASTValue const &v): Location(*v.ast) {}
Location::Location(NNPtr<IR::ASTValue const> v): Location(*v->ast) {}
Location::Location(std::string::iterator start, std::string::iterator end, NNPtr<File const> file): start(start), end(end), file(*file) {}

static Location locFromAST(ASTNS::AST &ast) {
    Maybe<Location const> &maybeStart = ast.start();
    Maybe<Location const> &maybeEnd = ast.end();

   auto withOp = [] (Location const &l) -> Location const { return l; };
   auto noOp =   [] ()                  -> Location const { reportAbortNoh("get location of ast with missing location info"); };
    Location const
        start (maybeStart.match<Location const>(withOp, noOp)),
        end   (maybeEnd  .match<Location const>(withOp, noOp));

    return Location(start.start, end.end, ast.file);
}
Location::Location(NNPtr<ASTNS::AST> ast): Location(locFromAST(*ast)) {}
Location::Location(ASTNS::AST &ast): Location(locFromAST(ast)) {}
// Error methods {{{1
Error::Error(MsgType type, Location const &location, std::string const &code, std::string const &name):
    type(type), location(location),
    code(code), name(name) {}
Error& Error::underline(Underline const &underline) {
    underlines.push_back(underline);
    return *this;
}
// Underline message methods {{{1
Underline::Underline(Location const &location, char ch): location(location), ch(ch) {}
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
void reportAbortNoh(std::string const &message) {
    std::cerr << "!!! Unrecoverable brokenness discovered in compiler !!!: " << message << std::endl;
    std::cerr << "!!! this is a bug - whether or not it has a bug report is unknown" << std::endl;
    std::cerr << "!!! bugs can be reported on the Katsel GitHub page: https://github.com/hpj2ltxry43b/katsel/issues" << std::endl;
    std::cerr << "!!! please search far and wide (on the GitHub page) before reporting a bug, so that there are no duplicate bug reports!" << std::endl;
    std::cerr << "Aborting..." << std::endl;
    std::abort();
}
void invalidTok(std::string const &name, Token const &underline) {
    reportAbortNoh(format("invalid token for %: \"%\"", name, underline));
}
void calledWithOpTyNEthis(std::string const &classN, std::string const &fnn, std::string const &opname) {
    reportAbortNoh(format("%::% called with % type != this", classN, fnn, opname));
}
void outOSwitchDDefaultLab(std::string const &fnn, Location const &highlight) {
    reportAbortNoh(format("% went out of switch despite default label", fnn));
}
void fCalled(std::string const &fnn) {
    reportAbortNoh(format("% called", fnn));
}
void outOSwitchNoh(std::string const &fnn) {
    reportAbortNoh(format("% went out of switch", fnn));
}
