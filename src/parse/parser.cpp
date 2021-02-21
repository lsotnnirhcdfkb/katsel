#include <memory>

#include <functional>
#include <map>
#include <type_traits>

#include "parse/parser.h"
#include "lex/lexer.h"
#include "ast/ast.h"
#include "message/errmsgs.h"
#include "lex/token.h"

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
        // TODO: for if - else if chain rules (ie stmt, type), pass consumed token into function
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

            std::vector<std::unique_ptr<ASTNS::ParamB>> params;
            if (!Tokens::is<Tokens::CParen>(peek().value)) {
                do {
                    TRY(p, std::unique_ptr<ASTNS::FunctionDecl>, param());
                    params.push_back(std::move(p));
                } while (consume_if<Tokens::Comma>());
            }

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
                    [&stop] (Maybe<Located<TokenData>> const &, Located<TokenData> const &peek) {
                        if (Tokens::is<Tokens::Fun>(peek.value)) return true;
                        else return stop(peek);
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
                [&stop] (Maybe<Located<TokenData>> const &prev, Located<TokenData> const &next) {
                    if (prev.has() && Tokens::is<Tokens::Newline>(prev.get().value))
                        return true;
                    else if (Tokens::is<Tokens::Var>(next.value) || Tokens::is<Tokens::Return>(next.value))
                        return true;
                    else
                        return stop(next);
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
                TRY(inner_initializer, std::unique_ptr<ASTNS::VarStmt>, expr(Precedence::NONE));
                initializer = std::move(inner_initializer);
            }

            TRY(line_ending, std::unique_ptr<ASTNS::VarStmt>, line_ending());

            Span stmt_span (var_tok.start, line_ending.end);
            return std::make_unique<ASTNS::VarStmt>(stmt_span, std::move(type), mut, name, eq_tok, std::move(initializer));
        }
        Maybe<std::unique_ptr<ASTNS::RetStmt>> ret_stmt() {
            Span ret_tok = prev().get().span;
            TRY(val, std::unique_ptr<ASTNS::RetStmt>, expr(Precedence::NONE));
            TRY(line_ending, std::unique_ptr<ASTNS::RetStmt>, line_ending());
            Span total_span (ret_tok.start, line_ending.end);
            return std::make_unique<ASTNS::RetStmt>(total_span, std::move(val));
        }
        Maybe<std::unique_ptr<ASTNS::ExprStmt>> expr_stmt() {
            TRY(expr, std::unique_ptr<ASTNS::ExprStmt>, expr(Precedence::NONE));
            // TODO: blocked exprs do not need line endings
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

            return inside_braces;
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

            return inside;
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
            TRY(path, std::unique_ptr<ASTNS::PathType>, path_no_first());
            return std::make_unique<ASTNS::PathType>(path->span(), std::move(path));
        }
        // params {{{2
        Maybe<std::unique_ptr<ASTNS::ParamB>> param() {
            if (Tokens::is<Tokens::Identifier>(peek().value) ||
                Tokens::is<Tokens::Mut>(peek().value))
                return normal_param();
            else if (Tokens::is<Tokens::Star>(peek().value) ||
                Tokens::is<Tokens::This>(peek().value))
                return this_param();
            else {
                ERR_EXPECTED(peek().span, "parameter");
                return Maybe<std::unique_ptr<ASTNS::ParamB>>();
            }
        }
        Maybe<std::unique_ptr<ASTNS::ThisParam>> this_param() {
            Span star_or_iden = peek().span;

            bool ptr = consume_if<Tokens::Star>();
            bool mut;
            if (ptr)
                mut = consume_if<Tokens::Mut>();

            TRY(this_tok, std::unique_ptr<ASTNS::ThisParam>, expect<Tokens::This>("'this'"));

            Span end_span = prev().get().span;

            return std::make_unique<ASTNS::ThisParam>(Span(star_or_iden.start, end_span.end), ptr, mut);
        }
        Maybe<std::unique_ptr<ASTNS::Param>> normal_param() {
            bool mut = consume_if<Tokens::Mut>();

            Maybe<Span> mut_loc = mut ? prev().get().span : Maybe<Span>();

            TRY(name, std::unique_ptr<ASTNS::Param>, expect<Tokens::Identifier>("parameter name"));
            TRY(type, std::unique_ptr<ASTNS::Param>, type_annotation("parameter type"));

            Span total_loc (
                mut_loc.has() ? mut_loc.get().start : name.span.start,
                type->span().get().end
            );

            return std::make_unique<ASTNS::Param>(total_loc, std::move(type), name, mut);
        }
        // expr {{{2
        // tables {{{3
        enum class Precedence {
            NONE = 0,
            ASSIGN = 1,
            BIN_OR = 2,
            BIN_AND = 3,
            COMP_EQ = 4,
            COMP_LGT = 5,
            BIT_XOR = 6,
            BIT_OR = 7,
            BIT_AND = 7,
            BIT_SHIFT = 8,
            ADD = 9,
            MULT = 10,
            CAST = 11,
            UNARY = 12,
            CALL_FIELD_METHOD = 13,
        };

        using PrefixParseFun = Maybe<std::unique_ptr<ASTNS::Expr>> (Parser::*)(Located<TokenData> const &);
        using InfixParseFun = Maybe<std::unique_ptr<ASTNS::Expr>> (Parser::*)(std::unique_ptr<ASTNS::Expr>, Located<TokenData> const &);
        std::map<size_t, PrefixParseFun> prefix_parsers {
            {Tokens::index_of<Tokens::OParen>    , &Parser::primary_expr},
            {Tokens::index_of<Tokens::FloatLit>  , &Parser::primary_expr},
            {Tokens::index_of<Tokens::IntLit>    , &Parser::primary_expr},
            {Tokens::index_of<Tokens::CharLit>   , &Parser::primary_expr},
            {Tokens::index_of<Tokens::StringLit> , &Parser::primary_expr},
            {Tokens::index_of<Tokens::BoolLit>   , &Parser::primary_expr},
            {Tokens::index_of<Tokens::This>      , &Parser::primary_expr},
            {Tokens::index_of<Tokens::OParen>    , &Parser::primary_expr},

            {Tokens::index_of<Tokens::Tilde>     , &Parser::unary_expr},
            {Tokens::index_of<Tokens::Minus>     , &Parser::unary_expr},
            {Tokens::index_of<Tokens::Bang>      , &Parser::unary_expr},
            {Tokens::index_of<Tokens::Star>      , &Parser::unary_expr},
            {Tokens::index_of<Tokens::Amper>     , &Parser::addrof_expr},

            {Tokens::index_of<Tokens::Identifier>, &Parser::path_expr},
        };

        std::map<size_t, std::pair<Precedence, InfixParseFun>> infix_parsers {
            {Tokens::index_of<Tokens::Equal>         , {Precedence::ASSIGN           , &Parser::assign_expr}},
            {Tokens::index_of<Tokens::DoublePipe>    , {Precedence::BIN_OR           , &Parser::bin_expr}},
            {Tokens::index_of<Tokens::DoubleAmper>   , {Precedence::BIN_AND          , &Parser::bin_expr}},
            {Tokens::index_of<Tokens::BangEqual>     , {Precedence::COMP_EQ          , &Parser::bin_expr}},
            {Tokens::index_of<Tokens::DoubleEqual>   , {Precedence::COMP_EQ          , &Parser::bin_expr}},
            {Tokens::index_of<Tokens::Less>          , {Precedence::COMP_LGT         , &Parser::bin_expr}},
            {Tokens::index_of<Tokens::Greater>       , {Precedence::COMP_LGT         , &Parser::bin_expr}},
            {Tokens::index_of<Tokens::LessEqual>     , {Precedence::COMP_LGT         , &Parser::bin_expr}},
            {Tokens::index_of<Tokens::GreaterEqual>  , {Precedence::COMP_LGT         , &Parser::bin_expr}},
            {Tokens::index_of<Tokens::Caret>         , {Precedence::BIT_XOR          , &Parser::bin_expr}},
            {Tokens::index_of<Tokens::Pipe>          , {Precedence::BIT_OR           , &Parser::bin_expr}},
            {Tokens::index_of<Tokens::Amper>         , {Precedence::BIT_AND          , &Parser::bin_expr}},
            {Tokens::index_of<Tokens::DoubleLess>    , {Precedence::BIT_SHIFT        , &Parser::bin_expr}},
            {Tokens::index_of<Tokens::DoubleGreater> , {Precedence::BIT_SHIFT        , &Parser::bin_expr}},
            {Tokens::index_of<Tokens::Plus>          , {Precedence::ADD              , &Parser::bin_expr}},
            {Tokens::index_of<Tokens::Minus>         , {Precedence::ADD              , &Parser::bin_expr}},
            {Tokens::index_of<Tokens::Star>          , {Precedence::MULT             , &Parser::bin_expr}},
            {Tokens::index_of<Tokens::Slash>         , {Precedence::MULT             , &Parser::bin_expr}},
            {Tokens::index_of<Tokens::Percent>       , {Precedence::MULT             , &Parser::bin_expr}},
            {Tokens::index_of<Tokens::RightArrow>    , {Precedence::CAST             , &Parser::cast_expr}},
            {Tokens::index_of<Tokens::OParen>        , {Precedence::CALL_FIELD_METHOD, &Parser::call_expr}},
            {Tokens::index_of<Tokens::Period>        , {Precedence::CALL_FIELD_METHOD, &Parser::field_or_method_call_expr}},
        };
        // main expr function {{{3
        Maybe<std::unique_ptr<ASTNS::Expr>> expr(Precedence prec) {
            auto next (peek());
            consume();

            auto pf = prefix_parsers.find(next.value.index());
            if (pf == prefix_parsers.end()) {
                ERR_EXPECTED(next.span, "expression");
                return Maybe<std::unique_ptr<ASTNS::Expr>>();
            }

            TRY(left, std::unique_ptr<ASTNS::Expr>, (this->*(pf->second))(next));

            while (precedence_of(peek().value) > prec) {
                auto infix_token (peek());
                auto infix_parser = infix_parsers.find(infix_token.value.index());

                if (infix_parser == infix_parsers.end())
                    break;

                consume();
                TRY(_left, std::unique_ptr<ASTNS::Expr>, (this->*(infix_parser->second.second))(std::move(left), infix_token));
                left = std::move(_left);
            }

            return std::move(left);
        }
        Precedence precedence_of(TokenData tok) {
            size_t index = tok.index();
            auto infix_parser = infix_parsers.find(index);

            if (infix_parser == infix_parsers.end())
                return Precedence::NONE;
            else
                return infix_parser->second.first;
        }
        // if {{{3
        Maybe<std::unique_ptr<ASTNS::IfExpr>> if_expr();
        // while {{{3
        Maybe<std::unique_ptr<ASTNS::WhileExpr>> while_expr();
        // bin {{{3
        Maybe<std::unique_ptr<ASTNS::Expr>> bin_expr(std::unique_ptr<ASTNS::Expr> left, Located<TokenData> const &op) {
            TRY(right, std::unique_ptr<ASTNS::Expr>, expr(precedence_of(op.value)));
            Span total_span (left->span().get().start, right->span().get().end);

            switch (op.value.index()) {
#define MAKE(expr_ty, op_ty, op_val) \
    return std::make_unique<ASTNS::expr_ty>(total_span, std::move(left), Located<ASTNS::op_ty> { op.span, ASTNS::op_ty::op_val }, std::move(right))
                case Tokens::index_of<Tokens::DoublePipe>: MAKE(ShortCircuitExpr, ShortCircuitOperator, DOUBLEPIPE);
                case Tokens::index_of<Tokens::DoubleAmper>: MAKE(ShortCircuitExpr, ShortCircuitOperator, DOUBLEAMPER);

                case Tokens::index_of<Tokens::BangEqual>: MAKE(BinaryExpr, BinaryOperator, BANGEQUAL);
                case Tokens::index_of<Tokens::DoubleEqual>: MAKE(BinaryExpr, BinaryOperator, DOUBLEEQUAL);
                case Tokens::index_of<Tokens::Less>: MAKE(BinaryExpr, BinaryOperator, LESS);
                case Tokens::index_of<Tokens::Greater>: MAKE(BinaryExpr, BinaryOperator, GREATER);
                case Tokens::index_of<Tokens::LessEqual>: MAKE(BinaryExpr, BinaryOperator, LESSEQUAL);
                case Tokens::index_of<Tokens::GreaterEqual>: MAKE(BinaryExpr, BinaryOperator, GREATEREQUAL);
                case Tokens::index_of<Tokens::Caret>: MAKE(BinaryExpr, BinaryOperator, CARET);
                case Tokens::index_of<Tokens::Pipe>: MAKE(BinaryExpr, BinaryOperator, PIPE);
                case Tokens::index_of<Tokens::Amper>: MAKE(BinaryExpr, BinaryOperator, AMPER);
                case Tokens::index_of<Tokens::DoubleLess>: MAKE(BinaryExpr, BinaryOperator, DOUBLELESS);
                case Tokens::index_of<Tokens::DoubleGreater>: MAKE(BinaryExpr, BinaryOperator, DOUBLEGREATER);
                case Tokens::index_of<Tokens::Star>: MAKE(BinaryExpr, BinaryOperator, STAR);
                case Tokens::index_of<Tokens::Slash>: MAKE(BinaryExpr, BinaryOperator, SLASH);
                case Tokens::index_of<Tokens::Plus>: MAKE(BinaryExpr, BinaryOperator, PLUS);
                case Tokens::index_of<Tokens::Minus>: MAKE(BinaryExpr, BinaryOperator, MINUS);
#undef MAKE

                default:
                    report_abort_noh("unreachable code reached");
            }
        }

        Maybe<std::unique_ptr<ASTNS::Expr>> assign_expr(std::unique_ptr<ASTNS::Expr> left, Located<TokenData> const &op) {
            TRY(right, std::unique_ptr<ASTNS::Expr>, expr(Precedence::NONE));
            Span total_span (left->span().get().start, right->span().get().end);

            ASTNS::AssignOperator assign_op;
            switch (op.value.index()) {
                case Tokens::index_of<Tokens::Equal>:
                    assign_op = ASTNS::AssignOperator::EQUAL;
                    break;

                default:
                    report_abort_noh("unreachable code reached");
            }

            return std::make_unique<ASTNS::AssignmentExpr>(total_span, std::move(left), Located<ASTNS::AssignOperator> { op.span, assign_op }, std::move(right));
        }

        // cast {{{3
        Maybe<std::unique_ptr<ASTNS::Expr>> cast_expr(std::unique_ptr<ASTNS::Expr> operand, Located<TokenData> const &op) {
            TRY(type, std::unique_ptr<ASTNS::Expr>, type("cast target"));
            return std::make_unique<ASTNS::CastExpr>(Span(operand->span().get().start, type->span().get().end), std::move(type), std::move(operand));
        }
        // unary {{{3
        Maybe<std::unique_ptr<ASTNS::Expr>> unary_expr(Located<TokenData> const &prev) {
            TRY(operand, std::unique_ptr<ASTNS::Expr>, expr(Precedence::UNARY));

            Span span (prev.span.start, operand->span().get().end);

            ASTNS::UnaryOperator op;
            switch (prev.value.index()) {
                case Tokens::index_of<Tokens::Tilde>:
                    op = ASTNS::UnaryOperator::TILDE;
                    break;
                case Tokens::index_of<Tokens::Minus>:
                    op = ASTNS::UnaryOperator::MINUS;
                    break;
                case Tokens::index_of<Tokens::Bang>:
                    op = ASTNS::UnaryOperator::BANG;
                    break;

                case Tokens::index_of<Tokens::Star>:
                    // special case, this needs to return a DerefExpr
                    return std::make_unique<ASTNS::DerefExpr>(span, Located<Tokens::Star> { prev.span, Tokens::as<Tokens::Star>(prev.value) }, std::move(operand));

                default:
                    report_abort_noh("unreachable code reached");
            }

            return std::make_unique<ASTNS::UnaryExpr>(span, Located<ASTNS::UnaryOperator> { prev.span, op }, std::move(operand));
        }
        // addrof {{{3
        Maybe<std::unique_ptr<ASTNS::Expr>> addrof_expr(Located<TokenData> const &amper) {
            bool mut = consume_if<Tokens::Mut>();

            TRY(operand, std::unique_ptr<ASTNS::Expr>, expr(Precedence::UNARY));

            Span total (amper.span.start, operand->span().get().end);
            Located<Tokens::Amper> amper_tok { amper.span, Tokens::as<Tokens::Amper>(amper.value) };
            return std::make_unique<ASTNS::AddrofExpr>(total, amper_tok, std::move(operand), mut);
        }
        // call & field access & method call {{{3
        Maybe<std::unique_ptr<ASTNS::Expr>> call_expr(std::unique_ptr<ASTNS::Expr> callee, Located<TokenData> const &oparen) {
            std::vector<std::unique_ptr<ASTNS::Expr>> call_args;
            if (!Tokens::is<Tokens::CParen>(peek().value)) {
                TRY(_args, std::unique_ptr<ASTNS::Expr>, args());
                call_args = std::move(_args);
            }

            TRY(cparen, std::unique_ptr<ASTNS::Expr>, expect<Tokens::CParen>("')'"));

            Located<Tokens::OParen> oparen_downcasted { oparen.span, Tokens::as<Tokens::OParen>(oparen.value) };

            return std::make_unique<ASTNS::CallExpr>(Span(callee->span().get().start, cparen.span.end), std::move(callee), oparen_downcasted, std::move(call_args));
        }
        Maybe<std::unique_ptr<ASTNS::Expr>> field_or_method_call_expr(std::unique_ptr<ASTNS::Expr> operand, Located<TokenData> const &dot) {
            TRY(name, std::unique_ptr<ASTNS::Expr>, expect<Tokens::Identifier>("field or method name"));
            Location span_left (operand->span().get().start);

            Located<Tokens::Period> dot_downcasted { dot.span, Tokens::as<Tokens::Period>(dot.value) };

            if (consume_if<Tokens::OParen>()) {
                Located<Tokens::OParen> oparen_downcasted { prev().get().span, Tokens::as<Tokens::OParen>(prev().get().value) };

                std::vector<std::unique_ptr<ASTNS::Expr>> call_args;
                if (!Tokens::is<Tokens::CParen>(peek().value)) {
                    TRY(_args, std::unique_ptr<ASTNS::Expr>, args());
                    call_args = std::move(_args);
                }

                TRY(cparen, std::unique_ptr<ASTNS::Expr>, expect<Tokens::CParen>("')'"));

                return std::make_unique<ASTNS::MethodCallExpr>(Span(span_left, cparen.span.end), std::move(operand), dot_downcasted, name, oparen_downcasted, std::move(call_args));
            } else {
                return std::make_unique<ASTNS::FieldAccessExpr>(Span(span_left, name.span.end), std::move(operand), dot_downcasted, name);
            }
        }
        // args {{{3
        Maybe<std::vector<std::unique_ptr<ASTNS::Expr>>> args() {
            std::vector<std::unique_ptr<ASTNS::Expr>> res;

            do {
                TRY(arg, std::vector<std::unique_ptr<ASTNS::Expr>>, expr(Precedence::NONE));
                res.push_back(std::move(arg));
            } while (consume_if<Tokens::Comma>());

            return res;
        }
        // primary {{{3
        Maybe<std::unique_ptr<ASTNS::Expr>> primary_expr(Located<TokenData> const &prev) {
            switch (prev.value.index()) {
#define A(a, b) \
    case Tokens::index_of<Tokens::a>: \
        return std::make_unique<ASTNS::b>(prev.span, Located<Tokens::a> { prev.span, Tokens::as<Tokens::a>(prev.value) });
                A(BoolLit, BoolLit)
                A(FloatLit, FloatLit)
                A(IntLit, IntLit)
                A(CharLit, CharLit)
                A(StringLit, StringLit)
                A(This, ThisExpr)
#undef A

                case Tokens::index_of<Tokens::OParen>: {
                    TRY(e, std::unique_ptr<ASTNS::Expr>, expr(Precedence::NONE));
                    TRY(close, std::unique_ptr<ASTNS::Expr>, expect<Tokens::CParen>("')'"));
                    return std::move(e);
               }

                default:
                    report_abort_noh("unreachable code reached");
            }
        }
        // path {{{3
        Maybe<std::unique_ptr<ASTNS::Expr>> path_expr(Located<TokenData> const &first) {
            TRY(path, std::unique_ptr<ASTNS::Expr>, path( Located<Tokens::Identifier> { first.span, Tokens::as<Tokens::Identifier>(first.value) }));
            return std::make_unique<ASTNS::PathExpr>(path->span(), std::move(path));
        }
        // paths {{{2
        Maybe<std::unique_ptr<ASTNS::Path>> path_no_first() {
            TRY(first, std::unique_ptr<ASTNS::Path>, expect<Tokens::Identifier>("path"));
            return path(first);
        }
        Maybe<std::unique_ptr<ASTNS::Path>> path(Located<Tokens::Identifier> const &first) {
            std::vector<Located<Tokens::Identifier>> segments { first };
            while (consume_if<Tokens::DoubleColon>()) {
                TRY(seg, std::unique_ptr<ASTNS::Path>, expect<Tokens::Identifier>("path segment"))
                segments.push_back(seg);
            }

            return std::make_unique<ASTNS::Path>(span_from_vec(segments), std::move(segments));
        }
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

            bool lastboom = false;
            while (true) {
                Located<TokenData> cur (lexer.next_token());

                // TODO: janky code should become less janky
                if (Tokens::is<Tokens::Error>(cur.value)) {
                    errored = true;
                    (*Tokens::as<Tokens::Error>(cur.value).errf)(cur.span);
                } else if (lastboom && Tokens::is<Tokens::Newline>(cur.value))
                    ;
                else if (Tokens::is<Tokens::Boom>(cur.value))
                    lastboom = true;
                else {
                    lastboom = false;
                    next_token = cur;
                    return;
                }
            }
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
        template <typename T>
        Maybe<Span> span_from_vec(std::vector<Located<T>> const &vec) {
            Maybe<Location> start;
            Maybe<Location> end;

            for (auto i = vec.cbegin(); i != vec.cend(); ++i) {
                Maybe<Span const> const &i_span = i->span;
                if (i_span.has()) {
                    start = i_span.get().start;
                    break;
                }
            }
            for (auto i = vec.crbegin(); i != vec.crend(); ++i) {
                Maybe<Span const> const &i_span = i->span;
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

            return things;
        }
        // }}}1
    };
}

Maybe<std::unique_ptr<ASTNS::CU>> Parse::parse(Lexer &l, File &sourcefile) {
    Parser p (l, sourcefile);
    return p.parse();
}
