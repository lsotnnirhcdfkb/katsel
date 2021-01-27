#include "utils/format.h"
#include "utils/location.h"
#include "utils/maybe.h"
#include "utils/assert.h"
#include "ir/value.h"
#include "ast/ast.h"

Location::Location(std::string::const_iterator iter, int line, int column, NNPtr<File const> file):
    iter(iter), line(line), column(column), file(*file) {}

static Span loc_from_ast(ASTNS::AST const &ast) {
    Maybe<Span const> const &maybe_span = ast.span();

    return maybe_span.match<Span const>(
            [] (Span const &l) -> Span const { return l; },
            [] ()              -> Span const { report_abort_noh("get span of ast with missing span"); });
}

Span::Span(ASTNS::AST const &ast): Span(loc_from_ast(ast)) {}
Span::Span(IR::ASTValue const &v): Span(*v.ast) {}

Span::Span(Location const &start, Location const &end): start(start), end(end), file(start.file) {
    ASSERT(start.file == end.file);
}
