#include <memory>

#include <functional>

#include "parse/parser.h"
#include "lex/lexer.h"
#include "ast/ast.h"
#include "message/errmsgs.h"
#include "lex/token.h"
#include <type_traits>

namespace {
    class Parser {
    public:
        // Parser code {{{1
        // constructor {{{2
        Parser(Lexer &l, File &source):
            lexer(l),
            source(source),
            errored(false),
            next_token(l.next_token()) {
        }
        // TODO: for if - else if chain rules, pass consumed token into function
        // entry point {{{2
        Maybe<std::unique_ptr<ASTNS::CU>> parse() {
            auto decls =
                thing_list_no_separator<std::unique_ptr<ASTNS::Decl>>(
                    [] (Located<TokenData> const &) {
                        return false; // stop predicate: never stop until reached end
                    },
                    [] (Maybe<Located<TokenData>> const &prev, Located<TokenData> const &peek) {
                        if (Tokens::is<Tokens::Fun>(peek.value) || Tokens::is<Tokens::Impl>(peek.value)) {
                            return true;
                        } else {
                            return false;
                        }
                    }, // sync predicate
                    &Parser::decl
                );

            if (errored) {
                return Maybe<std::unique_ptr<ASTNS::CU>>();
            } else {
                return std::make_unique<ASTNS::CU>(span_from_vec(decls), std::move(decls));
            }
        }
        // typedefs/using {{{2
        using TokenPredicate = std::function<bool(Located<TokenData> const &)>;
        // macros {{{2
#define TRY(name, ret_type, expr) \
    auto m_##name = expr; \
    if (!m_##name.has()) return Maybe<ret_type>(); \
    auto &name = m_##name.get();
        // declarations {{{2
        Maybe<std::unique_ptr<ASTNS::Decl>> decl() {
            if (consume_if<Tokens::Fun>())
                return function_decl();
            else if (consume_if<Tokens::Impl>())
                return impl_decl();
            else {
                ERR_EXPECTED_DECL(peek().span);
                return Maybe<std::unique_ptr<ASTNS::Decl>>();
            }
        }
        // function {{{3
        Maybe<std::unique_ptr<ASTNS::FunctionDecl>> function_decl() {
            Span fun_tok = prev().get().span;

            TRY(name, std::unique_ptr<ASTNS::FunctionDecl>, expect<Tokens::Identifier>("function name"));

            TRY(oparen, std::unique_ptr<ASTNS::FunctionDecl>, expect<Tokens::OParen>("'('"));
            // TODO: parameters
            // TODO: use "unclosed (" instead of "expected ')'"
            TRY(cparen, std::unique_ptr<ASTNS::FunctionDecl>, expect<Tokens::CParen>("')'"));

            TRY(ret_type, std::unique_ptr<ASTNS::FunctionDecl>, type_annotation("function return type"));

            // TODO: parse function declaration without definition

            TRY(body, std::unique_ptr<ASTNS::FunctionDecl>, blocked(&Parser::stmt_list));

            TRY(maybe_line_end, std::unique_ptr<ASTNS::FunctionDecl>, optional_line_ending());

            Span span (fun_tok.start, ret_type->span().has() ? ret_type->span().get().end : cparen.span.end);
            return std::make_unique<ASTNS::FunctionDecl>(span, std::move(ret_type), name, std::vector<std::unique_ptr<ASTNS::ParamB>> {}, std::move(body));
        }
        // impl {{{3
        Maybe<std::unique_ptr<ASTNS::ImplDecl>> impl_decl() {
            Span impl_tok = prev().get().span;

            TRY(type, std::unique_ptr<ASTNS::ImplDecl>, type("implementation type"));

            TRY(body, std::unique_ptr<ASTNS::ImplDecl>, blocked(&Parser::impl_body));

            Span span (impl_tok.start, type->span().get().end);
            return std::make_unique<ASTNS::ImplDecl>(span, std::move(type), std::move(body));
        }
        // body {{{4
        std::vector<std::unique_ptr<ASTNS::ImplMember>> impl_body(TokenPredicate stop) {
            return thing_list_no_separator<std::unique_ptr<ASTNS::ImplMember>>(
                    stop, // stop predicate
                    [] (Maybe<Located<TokenData>> const &, Located<TokenData> const &peek) {
                        if (Tokens::is<Tokens::Fun>(peek.value)) return true;
                        else return false;
                    }, // synchronization predicate
                    &Parser::impl_member
                );
        }
        // impl member {{{4
        Maybe<std::unique_ptr<ASTNS::ImplMember>> impl_member() {
            if (consume_if<Tokens::Fun>()) {
                TRY(fun_decl, std::unique_ptr<ASTNS::ImplMember>, function_decl());
                return std::make_unique<ASTNS::FunctionImplMember>(fun_decl->span(), std::move(fun_decl));
            } else {
                ERR_EXPECTED_IMPL_MEMBER(peek().span);
                return Maybe<std::unique_ptr<ASTNS::ImplMember>>();
            }
        }
        // statements {{{2
        Maybe<std::unique_ptr<ASTNS::Block>> stmt_list(TokenPredicate stop) {
            auto stmts = thing_list_no_separator<std::unique_ptr<ASTNS::Stmt>>(
                stop,
                [] (Maybe<Located<TokenData>> const &prev, Located<TokenData> const &next) {
                    if (prev.has() && Tokens::is<Tokens::Newline>(prev.get().value))
                        return true;
                    else if (Tokens::is<Tokens::Var>(next.value) || Tokens::is<Tokens::Return>(next.value))
                        return true;
                    else
                        return false;
                },
                &Parser::stmt
            );
            return std::make_unique<ASTNS::Block>(span_from_vec(stmts), std::move(stmts));
        }

        Maybe<std::unique_ptr<ASTNS::Stmt>> stmt() {
            if (consume_if<Tokens::Var>())
                return var_stmt();
            else if (consume_if<Tokens::Return>())
                return ret_stmt();
            else
                return expr_stmt();
        }
        Maybe<std::unique_ptr<ASTNS::VarStmt>> var_stmt() {
            Span var_tok = prev().get().span;

            bool mut = consume_if<Tokens::Mut>();

            TRY(name, std::unique_ptr<ASTNS::VarStmt>, expect<Tokens::Identifier>("variable name"));
            TRY(type, std::unique_ptr<ASTNS::VarStmt>, type_annotation("variable type"));

            std::unique_ptr<ASTNS::Expr> initializer = nullptr;
            Maybe<Located<Tokens::Equal>> eq_tok;
            if (consume_if<Tokens::Equal>()) {
                auto prev_tok = prev().get();
                eq_tok = Located<Tokens::Equal> { prev_tok.span, Tokens::as<Tokens::Equal>(prev_tok.value) };
                TRY(inner_initializer, std::unique_ptr<ASTNS::VarStmt>, expr());
                initializer = std::move(inner_initializer);
            }

            TRY(line_ending, std::unique_ptr<ASTNS::VarStmt>, line_ending());

            Span stmt_span (var_tok.start, line_ending.end);
            return std::make_unique<ASTNS::VarStmt>(stmt_span, std::move(type), mut, name, eq_tok, std::move(initializer));
        }
        Maybe<std::unique_ptr<ASTNS::RetStmt>> ret_stmt() {
            Span ret_tok = prev().get().span;
            TRY(val, std::unique_ptr<ASTNS::RetStmt>, expr());
            TRY(line_ending, std::unique_ptr<ASTNS::RetStmt>, line_ending());
            Span total_span (ret_tok.start, line_ending.end);
            return std::make_unique<ASTNS::RetStmt>(total_span, std::move(val));
        }
        Maybe<std::unique_ptr<ASTNS::ExprStmt>> expr_stmt() {
            TRY(expr, std::unique_ptr<ASTNS::ExprStmt>, expr());
            TRY(line_ending, std::unique_ptr<ASTNS::ExprStmt>, line_ending());
            return std::make_unique<ASTNS::ExprStmt>(expr->span(), std::move(expr));
        }
        // line endings {{{2
        Maybe<Span> line_ending() {
            if (consume_if<Tokens::Semicolon>()) {
                Span semi = prev().get().span;
                if (consume_if<Tokens::Semicolon>()) {
                    Span newl = prev().get().span;
                    return Span(semi.start, newl.end);
                } else {
                    return semi;
                }
            } else if (consume_if<Tokens::Newline>()) {
                Span newl = prev().get().span;
                return newl;
            } else {
                ERR_EXPECTED(peek().span, "line ending");
                return Maybe<Span>();
            }
        }

        // this Maybe is not for error handling, rather for the fact that optional_line_ending() doesn't always return a value
        Maybe<Span> optional_line_ending() {
            if (Tokens::is<Tokens::Newline>(peek().value) || Tokens::is<Tokens::Semicolon>(peek().value)) {
                return line_ending();
            } else {
                return Maybe<Span>();
            }
        }
        // blocks indented/braced {{{2
        template <typename T>
        struct UnwrapParseRes {
            using Result = T;
        };
        template <typename T>
        struct UnwrapParseRes<Maybe<T>> {
            using Result = T;
        };

        template <typename ParseFun, typename ... Args>
        using MaybeUnwrappedFunParseRes =
            Maybe<typename UnwrapParseRes<std::invoke_result_t<ParseFun, Parser *, TokenPredicate, Args...>>::Result>;
        // braced {{{3
        template <typename ParseFun, typename ... Args>
        MaybeUnwrappedFunParseRes<ParseFun, Args...> braced(ParseFun fun, Args &&...args) {
            enum class Braces {
                BRACE,
                BRACE_NL,
                BRACE_NL_IND
            };

            using FuncRet = typename UnwrapParseRes<std::invoke_result_t<ParseFun, Parser *, TokenPredicate, Args...>>::Result;

            TRY(obrace, FuncRet, expect<Tokens::OBrace>("'{'"));
            Braces braces = Braces::BRACE;

            if (consume_if<Tokens::Newline>()) {
                braces = Braces::BRACE_NL;
                if (consume_if<Tokens::Indent>()) {
                    braces = Braces::BRACE_NL_IND;
                }
            }

            auto stop_pred = [braces] (Located<TokenData> const &next) {
                switch (braces) {
                    case Braces::BRACE:
                    case Braces::BRACE_NL:
                        return Tokens::is<Tokens::CBrace>(next.value);

                    case Braces::BRACE_NL_IND:
                        return Tokens::is<Tokens::Dedent>(next.value);
                }
            };

            auto inside_braces = std::invoke(fun, this, stop_pred, args...);

            if (braces == Braces::BRACE_NL_IND)
                expect<Tokens::Dedent>("dedent");

            expect<Tokens::CBrace>("'}'");

            return std::move(inside_braces);
        }
        // indented {{{3
        template <typename ParseFun, typename ... Args>
        MaybeUnwrappedFunParseRes<ParseFun, Args...> indented(ParseFun fun, Args &&...args) {
            using FuncRet = typename UnwrapParseRes<std::invoke_result_t<ParseFun, Parser *, TokenPredicate, Args...>>::Result;

            TRY(nl, FuncRet, expect<Tokens::Newline>("newline"));
            TRY(indent, FuncRet, expect<Tokens::Indent>("indent"));

            auto inside = std::invoke(fun, this,
                [] (Located<TokenData> const &next) {
                    return Tokens::is<Tokens::Dedent>(next.value);
                },
                args...);

            TRY(dedent, FuncRet, expect<Tokens::Dedent>("dedent"));

            return std::move(inside);
        }
        // both {{{3
        template <typename ParseFun, typename ... Args>
        MaybeUnwrappedFunParseRes<ParseFun, Args...> blocked(ParseFun fun, Args &&...args) {
            using FuncRet = typename UnwrapParseRes<std::invoke_result_t<ParseFun, Parser *, TokenPredicate, Args...>>::Result;

            if (Tokens::is<Tokens::Newline>(peek().value)) {
                return indented(fun, args...);
            } else if (Tokens::is<Tokens::OBrace>(peek().value)) {
                return braced(fun, args...);
            } else {
                ERR_EXPECTED(peek().span, "blocked"); // TODO: better message
                return FuncRet();
            }
        }
        // types {{{2
        Maybe<std::unique_ptr<ASTNS::Type>> type_annotation(std::string const &what) {
            TRY(colon, std::unique_ptr<ASTNS::Type>, expect<Tokens::Colon>(":"));
            TRY(ty, std::unique_ptr<ASTNS::Type>, type(what));
            return std::move(ty);
        }
        Maybe<std::unique_ptr<ASTNS::Type>> type(std::string const &what) {
            if (consume_if<Tokens::Star>())
                return pointer_type();
            else if (consume_if<Tokens::This>())
                return this_type();
            else
                return path_type(what);
        }
        // pointer {{{3
        Maybe<std::unique_ptr<ASTNS::PointerType>> pointer_type() {
            Span star = prev().get().span;
            bool mut = consume_if<Tokens::Mut>();
            TRY(ty, std::unique_ptr<ASTNS::PointerType>, type("pointed type"));

            Span total (star.start, ty->span().get().end);
            return std::make_unique<ASTNS::PointerType>(total, mut, std::move(ty));
        }
        // this {{{3
        Maybe<std::unique_ptr<ASTNS::ThisType>> this_type() {
            Located<TokenData> prev_tok = prev().get();
            Located<Tokens::This> th { prev_tok.span, Tokens::as<Tokens::This>(prev_tok.value) };
            return std::make_unique<ASTNS::ThisType>(th.span, th);
        }
        // path {{{3
        Maybe<std::unique_ptr<ASTNS::PathType>> path_type(std::string const &what) {
            TRY(path, std::unique_ptr<ASTNS::PathType>, path(what));
            return std::make_unique<ASTNS::PathType>(path->span(), std::move(path));
        }
        // params {{{2
        Maybe<std::unique_ptr<ASTNS::ParamB>> param();
        Maybe<std::unique_ptr<ASTNS::ThisParam>> this_param();
        Maybe<std::unique_ptr<ASTNS::Param>> normal_param();
        // expr {{{2
        Maybe<std::unique_ptr<ASTNS::Expr>> expr();

        Maybe<std::unique_ptr<ASTNS::Expr>> blocked_expr();
        Maybe<std::unique_ptr<ASTNS::Expr>> not_blocked_expr();

        Maybe<std::unique_ptr<ASTNS::IfExpr>> if_expr();
        Maybe<std::unique_ptr<ASTNS::WhileExpr>> while_expr();

        Maybe<std::unique_ptr<ASTNS::Expr>> assignment_expr();
        Maybe<std::unique_ptr<ASTNS::Expr>> bin_or_expr();
        Maybe<std::unique_ptr<ASTNS::Expr>> bin_and_expr();
        Maybe<std::unique_ptr<ASTNS::Expr>> comp_eq_expr();
        Maybe<std::unique_ptr<ASTNS::Expr>> comp_lgt_expr();
        Maybe<std::unique_ptr<ASTNS::Expr>> bit_xor_expr();
        Maybe<std::unique_ptr<ASTNS::Expr>> bit_or_expr();
        Maybe<std::unique_ptr<ASTNS::Expr>> bit_and_expr();
        Maybe<std::unique_ptr<ASTNS::Expr>> bit_shift_expr();
        Maybe<std::unique_ptr<ASTNS::Expr>> addition_expr();
        Maybe<std::unique_ptr<ASTNS::Expr>> mult_expr();
        Maybe<std::unique_ptr<ASTNS::Expr>> cast_expr();
        Maybe<std::unique_ptr<ASTNS::Expr>> unary_expr();
        Maybe<std::unique_ptr<ASTNS::Expr>> call_expr();
        Maybe<std::unique_ptr<ASTNS::Expr>> field_access_expr();
        Maybe<std::unique_ptr<ASTNS::Expr>> method_call_expr();
        Maybe<std::unique_ptr<ASTNS::Expr>> primary_expr();
        Maybe<std::unique_ptr<ASTNS::Expr>> path_expr();
        // paths {{{2
        Maybe<std::unique_ptr<ASTNS::Path>> path(std::string const &what);
        // fields {{{2
        Lexer &lexer;
        File &source;

        bool errored;

        Maybe<Located<TokenData>> prev_token;
        Located<TokenData> next_token;
        // helpers {{{2
        // sync {{{3
        void synchronize(std::function<bool(Maybe<Located<TokenData>> const &, Located<TokenData> const &)> pred) {
            consume();

            while (!at_end()) {
                if (pred(prev(), peek()))
                    break;
                else
                    consume();
            }
        }
        // at_end {{{3
        bool at_end() {
            return Tokens::is<Tokens::_EOF>(peek().value);
        }
        // peek, prev {{{3
        Located<TokenData> &peek() {
            return next_token;
        }
        Maybe<Located<TokenData>> &prev() {
            return prev_token;
        }
        // consume, consume_if, expect {{{3
        void consume() {
            prev_token = next_token;
            next_token = lexer.next_token();
        }

        template <typename TokenType>
        bool consume_if() {
            if (peek().value.index() == Tokens::index_of<TokenType>) {
                consume();
                return true;
            } else {
                return false;
            }
        }

        template <typename TokenType>
        Maybe<Located<TokenType>> expect(std::string const &what) {
            if (peek().value.index() == Tokens::index_of<TokenType>) {
                auto tok = peek();
                auto tok_casted = Tokens::as<TokenType>(tok.value);
                auto tok_span = Located<TokenType> { tok.span, tok_casted };
                consume();

                return tok_span;
            } else {
                ERR_EXPECTED(peek().span, what);
                return Maybe<Located<TokenType>>();
            }
        }
        // span {{{3
        template <typename T>
        Maybe<Span> span_from_vec(std::vector<std::unique_ptr<T>> const &vec) {
            Maybe<Location> start;
            Maybe<Location> end;

            for (auto i = vec.cbegin(); i != vec.cend(); ++i) {
                Maybe<Span const> const &i_span = (*i)->span();
                if (i_span.has()) {
                    start = i_span.get().start;
                    break;
                }
            }
            for (auto i = vec.crbegin(); i != vec.crend(); ++i) {
                Maybe<Span const> const &i_span = (*i)->span();
                if (i_span.has()) {
                    end = i_span.get().end;
                    break;
                }
            }

            Maybe<Span> span = start.has() && end.has()
                ? Maybe<Span>(Span(start.get(), end.get()))
                : Maybe<Span>();

            return span;
        }
        // thing list {{{3
        template <typename Ret>
        std::vector<Ret> thing_list_no_separator(TokenPredicate stop, std::function<bool(Maybe<Located<TokenData>> const &, Located<TokenData> const &)> sync_pred, std::function<Maybe<Ret>(Parser *)> thing_fun) {
            std::vector<Ret> things;
            while (!at_end() && !stop(peek())) {
                Maybe<Ret> thing = thing_fun(this);
                if (thing.has())
                    things.push_back(std::move(thing.get()));
                else {
                    errored = true;
                    synchronize(sync_pred);
                }
            }

            return std::move(things);
        }
        // }}}1
    };
}

Maybe<std::unique_ptr<ASTNS::CU>> Parse::parse(Lexer &l, File &sourcefile) {
    Parser p (l, sourcefile);
    return p.parse();
}
