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
                    [] (Located<Token> const &) {
                        return false; // stop predicate: never stop until reached end
                    },
                    [] (Maybe<Located<Token>> const &prev, Located<Token> const &peek) {
                        if (peek.value.is<TokenType::Fun>() || peek.value.is<TokenType::Impl>()) {
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
                // fallback span is the eof token span
                return std::make_unique<ASTNS::CU>(span_from_vec(decls, peek().span), std::move(decls));
            }
        }
        // typedefs/using {{{2
        using TokenPredicate = std::function<bool(Located<Token> const &)>;
        // macros {{{2
#define TRY(name, ret_type, expr) \
    auto m_##name = expr; \
    if (!m_##name.has()) return Maybe<ret_type>(); \
    auto &name = m_##name.get();
        // declarations {{{2
        Maybe<std::unique_ptr<ASTNS::Decl>> decl() {
            if (consume_if<TokenType::Fun>())
                return function_decl();
            else if (consume_if<TokenType::Impl>())
                return impl_decl();
            else {
                Errors::Expected(peek().span, "declaration").report();
                return Maybe<std::unique_ptr<ASTNS::Decl>>();
            }
        }
        // function {{{3
        Maybe<std::unique_ptr<ASTNS::FunctionDecl>> function_decl() {
            Span fun_tok = prev().get().span;

            TRY(name, std::unique_ptr<ASTNS::FunctionDecl>, expect<TokenType::Identifier>("function name"));

            TRY(oparen, std::unique_ptr<ASTNS::FunctionDecl>, expect<TokenType::OParen>("'('"));

            std::vector<std::unique_ptr<ASTNS::ParamB>> params;
            if (!peek().value.is<TokenType::CParen>()) {
                do {
                    TRY(p, std::unique_ptr<ASTNS::FunctionDecl>, param());
                    params.push_back(std::move(p));
                } while (consume_if<TokenType::Comma>());
            }

            // TODO: use "unclosed (" instead of "expected ')'"
            TRY(cparen, std::unique_ptr<ASTNS::FunctionDecl>, expect<TokenType::CParen>("')'"));

            TRY(ret_type, std::unique_ptr<ASTNS::FunctionDecl>, type_annotation());

            // TODO: parse function declaration without definition

            TRY(body, std::unique_ptr<ASTNS::FunctionDecl>, blocked<true>(&Parser::stmt_list));

            optional_line_ending();

            Span span (join_span(fun_tok, ret_type->span()));
            return std::make_unique<ASTNS::FunctionDecl>(span, std::move(ret_type), Located<Token>(name, name.value.clone()), std::move(params), std::move(body));
        }
        // impl {{{3
        Maybe<std::unique_ptr<ASTNS::ImplDecl>> impl_decl() {
            Span impl_tok = prev().get().span;

            TRY(type, std::unique_ptr<ASTNS::ImplDecl>, type());

            TRY(body, std::unique_ptr<ASTNS::ImplDecl>, blocked(&Parser::impl_body));

            return std::make_unique<ASTNS::ImplDecl>(join_span(impl_tok, type->span()), std::move(type), std::move(body));
        }
        // body {{{4
        std::vector<std::unique_ptr<ASTNS::ImplMember>> impl_body(TokenPredicate stop) {
            return thing_list_no_separator<std::unique_ptr<ASTNS::ImplMember>>(
                    stop, // stop predicate
                    [&stop] (Maybe<Located<Token>> const &, Located<Token> const &peek) {
                        if (peek.value.is<TokenType::Fun>()) return true;
                        else return stop(peek);
                    }, // synchronization predicate
                    &Parser::impl_member
                );
        }
        // impl member {{{4
        Maybe<std::unique_ptr<ASTNS::ImplMember>> impl_member() {
            if (consume_if<TokenType::Fun>()) {
                TRY(fun_decl, std::unique_ptr<ASTNS::ImplMember>, function_decl());
                return std::make_unique<ASTNS::FunctionImplMember>(fun_decl->span(), std::move(fun_decl));
            } else {
                Errors::Expected(peek().span, "\'impl\' member").report();
                return Maybe<std::unique_ptr<ASTNS::ImplMember>>();
            }
        }
        // statements {{{2
        Maybe<std::unique_ptr<ASTNS::Block>> stmt_list(TokenPredicate stop, Span const &fallback_span) {
            auto stmts = thing_list_no_separator<std::unique_ptr<ASTNS::Stmt>>(
                stop,
                [&stop] (Maybe<Located<Token>> const &prev, Located<Token> const &next) {
                    if (prev.has() && prev.get().value.is<TokenType::Newline>())
                        return true;
                    else if (next.value.is<TokenType::Var>() || next.value.is<TokenType::Return>())
                        return true;
                    else
                        return stop(next);
                },
                &Parser::stmt
            );

            return std::make_unique<ASTNS::Block>(span_from_vec(stmts, fallback_span), std::move(stmts));
        }

        Maybe<std::unique_ptr<ASTNS::Stmt>> stmt() {
            if (consume_if<TokenType::Var>())
                return var_stmt();
            else if (consume_if<TokenType::Return>())
                return ret_stmt();
            else
                return expr_stmt();
        }
        Maybe<std::unique_ptr<ASTNS::VarStmt>> var_stmt() {
            Span var_tok = prev().get().span;

            bool mut = consume_if<TokenType::Mut>();

            TRY(name, std::unique_ptr<ASTNS::VarStmt>, expect<TokenType::Identifier>("variable name"));
            TRY(type, std::unique_ptr<ASTNS::VarStmt>, type_annotation());

            std::unique_ptr<ASTNS::Expr> initializer = nullptr;
            Maybe<Located<Token>> eq_tok;
            if (consume_if<TokenType::Equal>()) {
                auto &prev_tok = prev().get();
                eq_tok = Maybe<Located<Token>>(Located<Token>(prev_tok, prev_tok.value.clone()));

                TRY(inner_initializer, std::unique_ptr<ASTNS::VarStmt>, expr(Precedence::NONE));
                initializer = std::move(inner_initializer);
            }

            TRY(line_ending, std::unique_ptr<ASTNS::VarStmt>, line_ending());

            Span stmt_span (var_tok.start, line_ending.end);
            return std::make_unique<ASTNS::VarStmt>(stmt_span, std::move(type), mut, Located<Token>(name, name.value.clone()), std::move(eq_tok), std::move(initializer));
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

            if (dynamic_cast<ASTNS::IfExpr*   >(expr.get()) ||
                dynamic_cast<ASTNS::WhileExpr*>(expr.get()) ||
                dynamic_cast<ASTNS::Block*    >(expr.get())) {
                // exprs with blocks do not require a line ending
                optional_line_ending();
            } else {
                // non-blocked exprs do require a line ending
                TRY(line_ending, std::unique_ptr<ASTNS::ExprStmt>, line_ending());
            }

            return std::make_unique<ASTNS::ExprStmt>(expr->span(), std::move(expr));
        }
        // line endings {{{2
        Maybe<Span> line_ending() {
            if (consume_if<TokenType::Semicolon>()) {
                Span semi = prev().get().span;
                if (consume_if<TokenType::Newline>()) { // TODO: when lexer does not insert newline after ';', this will not be necessary
                    Span newl = prev().get().span;
                    return Span(semi.start, newl.end);
                } else {
                    return semi;
                }
            } else if (consume_if<TokenType::Newline>()) {
                Span newl = prev().get().span;
                return newl;
            } else {
                Errors::Expected(peek().span, "line ending").report();
                return Maybe<Span>();
            }
        }

        // this Maybe is not for error handling, rather for the fact that optional_line_ending() doesn't always return a value
        Maybe<Span> optional_line_ending() {
            if (peek().value.is<TokenType::Newline>() || peek().value.is<TokenType::Semicolon>()) {
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

        template <bool PassFallbackSpan, typename ParseFun, typename ... Args>
        struct ParseFunRes {
            using Result = std::invoke_result_t<ParseFun, Parser *, TokenPredicate, Args...>;
        };

        template <typename ParseFun, typename ... Args>
        struct ParseFunRes<true, ParseFun, Args...> {
            using Result = std::invoke_result_t<ParseFun, Parser *, TokenPredicate, Span, Args...>;
        };

        template <typename T>
        using UnwrapParseResT = typename UnwrapParseRes<T>::Result;

        template <bool PassFallbackSpan, typename ParseFun, typename ... Args>
        using ParseFunResT = typename ParseFunRes<PassFallbackSpan, ParseFun, Args...>::Result;

        template <bool PassFallbackSpan, typename ParseFun, typename ... Args>
        using MaybeUnwrappedFunParseRes =
            Maybe<UnwrapParseResT<ParseFunResT<PassFallbackSpan, ParseFun, Args...>>>;
        // braced {{{3
        template <bool PassFallbackSpan = false, typename ParseFun, typename ... Args>
        MaybeUnwrappedFunParseRes<PassFallbackSpan, ParseFun, Args...> braced(ParseFun fun, Args &&...args) {
            using FuncRet = UnwrapParseResT<ParseFunResT<PassFallbackSpan, ParseFun, Args...>>;

            TRY(obrace, FuncRet, expect<TokenType::OBrace>("'{'"));

            auto inside = [&] () {
                if constexpr (PassFallbackSpan)
                    return std::invoke(fun, this,
                        [] (Located<Token> const &next) {
                            return next.value.is<TokenType::CBrace>();
                        },
                        obrace.span,
                        args...);
                else
                    return std::invoke(fun, this,
                        [] (Located<Token> const &next) {
                            return next.value.is<TokenType::CBrace>();
                        },
                        args...);
            }();

            expect<TokenType::CBrace>("'}'");

            return inside;
        }
        // indented {{{3
        template <bool PassFallbackSpan = false, typename ParseFun, typename ... Args>
        MaybeUnwrappedFunParseRes<PassFallbackSpan, ParseFun, Args...> indented(ParseFun fun, Args &&...args) {
            using FuncRet = UnwrapParseResT<ParseFunResT<PassFallbackSpan, ParseFun, Args...>>;

            TRY(indent, FuncRet, expect<TokenType::Indent>("indent"));

            auto inside = [&] () {
                if constexpr (PassFallbackSpan)
                    return std::invoke(fun, this,
                        [] (Located<Token> const &next) {
                            return next.value.is<TokenType::Dedent>();
                        },
                        indent.span,
                        args...);
                else
                    return std::invoke(fun, this,
                        [] (Located<Token> const &next) {
                            return next.value.is<TokenType::Dedent>();
                        },
                        args...);
            }();

            TRY(dedent, FuncRet, expect<TokenType::Dedent>("dedent"));

            return inside;
        }
        // both {{{3
        template <bool PassFallbackSpan = false, typename ParseFun, typename ... Args>
        MaybeUnwrappedFunParseRes<PassFallbackSpan, ParseFun, Args...> blocked(ParseFun fun, Args &&...args) {
            if (peek().value.is<TokenType::Indent>()) {
                return indented<PassFallbackSpan>(fun, args...);
            } else if (peek().value.is<TokenType::OBrace>()) {
                return braced<PassFallbackSpan>(fun, args...);
            } else {
                Errors::Expected(peek().span, "blocked"); // TODO: better message
                return MaybeUnwrappedFunParseRes<PassFallbackSpan, ParseFun, Args...>();
            }
        }
        // types {{{2
        Maybe<std::unique_ptr<ASTNS::Type>> type_annotation() {
            TRY(colon, std::unique_ptr<ASTNS::Type>, expect<TokenType::Colon>(":"));
            TRY(ty, std::unique_ptr<ASTNS::Type>, type());
            return std::move(ty);
        }
        Maybe<std::unique_ptr<ASTNS::Type>> type() {
            if (consume_if<TokenType::Star>())
                return pointer_type();
            else if (consume_if<TokenType::This>())
                return this_type();
            else if (peek().value.is<TokenType::Identifier>())
                return path_type();
            else {
                Errors::Expected(peek().span, "type").report();
                return Maybe<std::unique_ptr<ASTNS::Type>>();
            }
        }
        // pointer {{{3
        Maybe<std::unique_ptr<ASTNS::PointerType>> pointer_type() {
            Span star = prev().get().span;
            bool mut = consume_if<TokenType::Mut>();
            TRY(ty, std::unique_ptr<ASTNS::PointerType>, type());

            return std::make_unique<ASTNS::PointerType>(join_span(star, ty->span()), mut, std::move(ty));
        }
        // this {{{3
        Maybe<std::unique_ptr<ASTNS::ThisType>> this_type() {
            Located<Token> &prev_tok = prev().get();
            return std::make_unique<ASTNS::ThisType>(prev_tok.span, Located<Token>(prev_tok.span, prev_tok.value.clone()));
        }
        // path {{{3
        Maybe<std::unique_ptr<ASTNS::PathType>> path_type() {
            TRY(path, std::unique_ptr<ASTNS::PathType>, path());
            return std::make_unique<ASTNS::PathType>(path->span(), std::move(path));
        }
        // params {{{2
        Maybe<std::unique_ptr<ASTNS::ParamB>> param() {
            if (peek().value.is<TokenType::Identifier>() ||
                peek().value.is<TokenType::Mut>())
                return normal_param();
            else if (peek().value.is<TokenType::Star>() ||
                peek().value.is<TokenType::This>())
                return this_param();
            else {
                Errors::Expected(peek().span, "parameter").report();
                return Maybe<std::unique_ptr<ASTNS::ParamB>>();
            }
        }
        Maybe<std::unique_ptr<ASTNS::ThisParam>> this_param() {
            Span star_or_iden = peek().span;

            bool ptr = consume_if<TokenType::Star>();
            bool mut;
            if (ptr)
                mut = consume_if<TokenType::Mut>();

            TRY(this_tok, std::unique_ptr<ASTNS::ThisParam>, expect<TokenType::This>("'this'"));

            Span end_span = prev().get().span;

            return std::make_unique<ASTNS::ThisParam>(Span(star_or_iden.start, end_span.end), ptr, mut);
        }
        Maybe<std::unique_ptr<ASTNS::Param>> normal_param() {
            bool mut = consume_if<TokenType::Mut>();
            Maybe<Span> mut_loc = mut ? prev().get().span : Maybe<Span>();

            TRY(name, std::unique_ptr<ASTNS::Param>, expect<TokenType::Identifier>("parameter name"));
            TRY(type, std::unique_ptr<ASTNS::Param>, type_annotation());

            return std::make_unique<ASTNS::Param>(join_span(mut_loc.has() ? mut_loc.get() : name.span, type->span()), std::move(type), Located<Token>(name, name.value.clone()), mut);
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

        using PrefixParseFun = Maybe<std::unique_ptr<ASTNS::Expr>> (Parser::*)();
        using InfixParseFun = Maybe<std::unique_ptr<ASTNS::Expr>> (Parser::*)(std::unique_ptr<ASTNS::Expr>);
        std::map<TokenType, PrefixParseFun> prefix_parsers = std::initializer_list<std::map<TokenType, PrefixParseFun>::value_type> {
            {TokenType::OParen    , &Parser::primary_expr},
            {TokenType::FloatLit  , &Parser::primary_expr},
            {TokenType::IntLit    , &Parser::primary_expr},
            {TokenType::CharLit   , &Parser::primary_expr},
            {TokenType::StringLit , &Parser::primary_expr},
            {TokenType::BoolLit   , &Parser::primary_expr},
            {TokenType::This      , &Parser::primary_expr},
            {TokenType::OParen    , &Parser::primary_expr},

            {TokenType::Tilde     , &Parser::unary_expr},
            {TokenType::Minus     , &Parser::unary_expr},
            {TokenType::Bang      , &Parser::unary_expr},
            {TokenType::Star      , &Parser::unary_expr},
            {TokenType::Amper     , &Parser::addrof_expr},

            {TokenType::Identifier, &Parser::path_expr},

            {TokenType::If        , &Parser::if_expr},
            {TokenType::While     , &Parser::while_expr},

            {TokenType::OBrace    , &Parser::braced_block_expr},
        };

        std::map<TokenType, std::pair<Precedence, InfixParseFun>> infix_parsers = std::initializer_list<std::map<TokenType, std::pair<Precedence, InfixParseFun>>::value_type> {
            {TokenType::Equal         , {Precedence::ASSIGN           , &Parser::assign_expr}},
            {TokenType::DoublePipe    , {Precedence::BIN_OR           , &Parser::bin_expr}},
            {TokenType::DoubleAmper   , {Precedence::BIN_AND          , &Parser::bin_expr}},
            {TokenType::BangEqual     , {Precedence::COMP_EQ          , &Parser::bin_expr}},
            {TokenType::DoubleEqual   , {Precedence::COMP_EQ          , &Parser::bin_expr}},
            {TokenType::Less          , {Precedence::COMP_LGT         , &Parser::bin_expr}},
            {TokenType::Greater       , {Precedence::COMP_LGT         , &Parser::bin_expr}},
            {TokenType::LessEqual     , {Precedence::COMP_LGT         , &Parser::bin_expr}},
            {TokenType::GreaterEqual  , {Precedence::COMP_LGT         , &Parser::bin_expr}},
            {TokenType::Caret         , {Precedence::BIT_XOR          , &Parser::bin_expr}},
            {TokenType::Pipe          , {Precedence::BIT_OR           , &Parser::bin_expr}},
            {TokenType::Amper         , {Precedence::BIT_AND          , &Parser::bin_expr}},
            {TokenType::DoubleLess    , {Precedence::BIT_SHIFT        , &Parser::bin_expr}},
            {TokenType::DoubleGreater , {Precedence::BIT_SHIFT        , &Parser::bin_expr}},
            {TokenType::Plus          , {Precedence::ADD              , &Parser::bin_expr}},
            {TokenType::Minus         , {Precedence::ADD              , &Parser::bin_expr}},
            {TokenType::Star          , {Precedence::MULT             , &Parser::bin_expr}},
            {TokenType::Slash         , {Precedence::MULT             , &Parser::bin_expr}},
            {TokenType::Percent       , {Precedence::MULT             , &Parser::bin_expr}},
            {TokenType::RightArrow    , {Precedence::CAST             , &Parser::cast_expr}},
            {TokenType::OParen        , {Precedence::CALL_FIELD_METHOD, &Parser::call_expr}},
            {TokenType::Period        , {Precedence::CALL_FIELD_METHOD, &Parser::field_or_method_call_expr}},
        };
        // main expr function {{{3
        Maybe<std::unique_ptr<ASTNS::Expr>> expr(Precedence prec) {
            auto &next (peek());

            auto pf = prefix_parsers.find(next.value.type());
            if (pf == prefix_parsers.end()) {
                Errors::Expected(next.span, "expression").report();
                return Maybe<std::unique_ptr<ASTNS::Expr>>();
            }

            TRY(left, std::unique_ptr<ASTNS::Expr>, (this->*(pf->second))());

            while (precedence_of(peek().value) > prec) {
                auto &infix_token (peek());
                auto infix_parser = infix_parsers.find(infix_token.value.type());

                if (infix_parser == infix_parsers.end())
                    break;

                TRY(_left, std::unique_ptr<ASTNS::Expr>, (this->*(infix_parser->second.second))(std::move(left)));
                left = std::move(_left);
            }

            return std::move(left);
        }
        Precedence precedence_of(Token const &tok) {
            TokenType index = tok.type();
            auto infix_parser = infix_parsers.find(index);

            if (infix_parser == infix_parsers.end())
                return Precedence::NONE;
            else
                return infix_parser->second.first;
        }
        // if {{{3
        Maybe<std::unique_ptr<ASTNS::Expr>> if_expr() {
            Located<Token> const &if_tok = assert_expect<TokenType::If>();

            TRY(cond, std::unique_ptr<ASTNS::Expr>, expr(Precedence::NONE));
            TRY(if_branch, std::unique_ptr<ASTNS::Expr>, blocked<true>(&Parser::stmt_list));

            Maybe<Located<Token>> else_tok;
            std::unique_ptr<ASTNS::Expr> else_branch;
            if (consume_if<TokenType::Else>()) {
                auto &prev_tok = prev().get();
                else_tok = Located<Token>(prev_tok, prev_tok.value.clone());

                if (peek().value.is<TokenType::If>()) {
                    TRY(_else_branch, std::unique_ptr<ASTNS::Expr>, if_expr());
                    else_branch = std::move(_else_branch);
                } else {
                    TRY(_else_branch, std::unique_ptr<ASTNS::Expr>, blocked<true>(&Parser::stmt_list));
                    else_branch = std::move(_else_branch);
                }
            }

            return std::make_unique<ASTNS::IfExpr>(join_span(if_tok.span, else_branch ? else_branch->span() : if_branch->span()), Located<Token>(if_tok, if_tok.value.clone()), std::move(else_tok), std::move(cond), std::move(if_branch), std::move(else_branch));
        }
        // while {{{3
        Maybe<std::unique_ptr<ASTNS::Expr>> while_expr() {
            Located<Token> const &while_tok = assert_expect<TokenType::While>();

            TRY(cond, std::unique_ptr<ASTNS::Expr>, expr(Precedence::NONE));
            TRY(body, std::unique_ptr<ASTNS::Expr>, blocked<true>(&Parser::stmt_list));

            return std::make_unique<ASTNS::WhileExpr>(join_span(while_tok.span, body->span()), std::move(cond), std::move(body));

        }
        // block expr {{{3
        Maybe<std::unique_ptr<ASTNS::Expr>> braced_block_expr() {
            return braced<true>(&Parser::stmt_list);
        }
        // bin {{{3
        Maybe<std::unique_ptr<ASTNS::Expr>> bin_expr(std::unique_ptr<ASTNS::Expr> left) {
            auto &op = peek();
            consume();

            TRY(right, std::unique_ptr<ASTNS::Expr>, expr(precedence_of(op.value)));
            Span const total_span (join_span(left->span(), right->span()));

            switch (op.value.type()) {
#define MAKE(expr_ty, op_ty, op_val) \
    return std::make_unique<ASTNS::expr_ty>(total_span, std::move(left), Located<ASTNS::op_ty>(op.span, ASTNS::op_ty::op_val), std::move(right))
                case TokenType::DoublePipe: MAKE(ShortCircuitExpr, ShortCircuitOperator, DOUBLEPIPE);
                case TokenType::DoubleAmper: MAKE(ShortCircuitExpr, ShortCircuitOperator, DOUBLEAMPER);

                case TokenType::BangEqual: MAKE(BinaryExpr, BinaryOperator, BANGEQUAL);
                case TokenType::DoubleEqual: MAKE(BinaryExpr, BinaryOperator, DOUBLEEQUAL);
                case TokenType::Less: MAKE(BinaryExpr, BinaryOperator, LESS);
                case TokenType::Greater: MAKE(BinaryExpr, BinaryOperator, GREATER);
                case TokenType::LessEqual: MAKE(BinaryExpr, BinaryOperator, LESSEQUAL);
                case TokenType::GreaterEqual: MAKE(BinaryExpr, BinaryOperator, GREATEREQUAL);
                case TokenType::Caret: MAKE(BinaryExpr, BinaryOperator, CARET);
                case TokenType::Pipe: MAKE(BinaryExpr, BinaryOperator, PIPE);
                case TokenType::Amper: MAKE(BinaryExpr, BinaryOperator, AMPER);
                case TokenType::DoubleLess: MAKE(BinaryExpr, BinaryOperator, DOUBLELESS);
                case TokenType::DoubleGreater: MAKE(BinaryExpr, BinaryOperator, DOUBLEGREATER);
                case TokenType::Star: MAKE(BinaryExpr, BinaryOperator, STAR);
                case TokenType::Slash: MAKE(BinaryExpr, BinaryOperator, SLASH);
                case TokenType::Percent: MAKE(BinaryExpr, BinaryOperator, PERCENT);
                case TokenType::Plus: MAKE(BinaryExpr, BinaryOperator, PLUS);
                case TokenType::Minus: MAKE(BinaryExpr, BinaryOperator, MINUS);
#undef MAKE

                default:
                    report_abort_noh("invalid binary operator");
            }
        }

        Maybe<std::unique_ptr<ASTNS::Expr>> assign_expr(std::unique_ptr<ASTNS::Expr> left) {
            auto &op = peek();
            consume();

            TRY(right, std::unique_ptr<ASTNS::Expr>, expr(Precedence::NONE));
            Span const total_span (join_span(left->span(), right->span()));

            ASTNS::AssignOperator assign_op;
            switch (op.value.type()) {
                case TokenType::Equal:
                    assign_op = ASTNS::AssignOperator::EQUAL;
                    break;

                default:
                    report_abort_noh("invalid assign operator");
            }

            return std::make_unique<ASTNS::AssignmentExpr>(total_span, std::move(left), Located<ASTNS::AssignOperator>(op.span, assign_op), std::move(right));
        }

        // cast {{{3
        Maybe<std::unique_ptr<ASTNS::Expr>> cast_expr(std::unique_ptr<ASTNS::Expr> operand) {
            assert_expect<TokenType::RightArrow>();
            TRY(type, std::unique_ptr<ASTNS::Expr>, type());
            return std::make_unique<ASTNS::CastExpr>(join_span(operand->span(), type->span()), std::move(type), std::move(operand));
        }
        // unary {{{3
        Maybe<std::unique_ptr<ASTNS::Expr>> unary_expr() {
            auto &prev = peek();
            consume();

            TRY(operand, std::unique_ptr<ASTNS::Expr>, expr(Precedence::UNARY));

            Span const span (join_span(prev.span, operand->span()));

            ASTNS::UnaryOperator op;
            switch (prev.value.type()) {
                case TokenType::Tilde:
                    op = ASTNS::UnaryOperator::TILDE;
                    break;
                case TokenType::Minus:
                    op = ASTNS::UnaryOperator::MINUS;
                    break;
                case TokenType::Bang:
                    op = ASTNS::UnaryOperator::BANG;
                    break;

                case TokenType::Star:
                    // special case, this needs to return a DerefExpr
                    return std::make_unique<ASTNS::DerefExpr>(span, Located<Token>(prev.span, prev.value.clone()), std::move(operand));

                default:
                    report_abort_noh("invalid unary operator");
            }

            return std::make_unique<ASTNS::UnaryExpr>(span, Located<ASTNS::UnaryOperator>(prev.span, op), std::move(operand));
        }
        // addrof {{{3
        Maybe<std::unique_ptr<ASTNS::Expr>> addrof_expr() {
            auto const &amper = assert_expect<TokenType::Amper>();

            bool mut = consume_if<TokenType::Mut>();

            TRY(operand, std::unique_ptr<ASTNS::Expr>, expr(Precedence::UNARY));

            Span const total (join_span(amper.span, operand->span()));
            return std::make_unique<ASTNS::AddrofExpr>(total, Located<Token>(amper, amper.value.clone()), std::move(operand), mut);
        }
        // call & field access & method call {{{3
        Maybe<std::unique_ptr<ASTNS::Expr>> call_expr(std::unique_ptr<ASTNS::Expr> callee) {
            auto const &oparen = assert_expect<TokenType::OParen>();

            std::vector<std::unique_ptr<ASTNS::Expr>> call_args;
            if (!peek().value.is<TokenType::CParen>()) {
                TRY(_args, std::unique_ptr<ASTNS::Expr>, args());
                call_args = std::move(_args);
            }

            TRY(cparen, std::unique_ptr<ASTNS::Expr>, expect<TokenType::CParen>("')'"));

            return std::make_unique<ASTNS::CallExpr>(join_span(callee->span(), cparen.span), std::move(callee), Located<Token>(oparen, oparen.value.clone()), std::move(call_args));
        }
        Maybe<std::unique_ptr<ASTNS::Expr>> field_or_method_call_expr(std::unique_ptr<ASTNS::Expr> operand) {
            auto const &dot = assert_expect<TokenType::Period>();

            TRY(name, std::unique_ptr<ASTNS::Expr>, expect<TokenType::Identifier>("field or method name"));

            if (consume_if<TokenType::OParen>()) {
                auto &oparen = prev().get();

                std::vector<std::unique_ptr<ASTNS::Expr>> call_args;
                if (!peek().value.is<TokenType::CParen>()) {
                    TRY(_args, std::unique_ptr<ASTNS::Expr>, args());
                    call_args = std::move(_args);
                }

                TRY(cparen, std::unique_ptr<ASTNS::Expr>, expect<TokenType::CParen>("')'"));

                return std::make_unique<ASTNS::MethodCallExpr>(join_span(operand->span(), cparen.span), std::move(operand), Located<Token>(dot, dot.value.clone()), Located<Token>(name, name.value.clone()), Located<Token>(oparen, oparen.value.clone()), std::move(call_args));
            } else {
                return std::make_unique<ASTNS::FieldAccessExpr>(join_span(operand->span(), name.span), std::move(operand), Located<Token>(dot, dot.value.clone()), Located<Token>(name, name.value.clone()));
            }
        }
        // args {{{3
        Maybe<std::vector<std::unique_ptr<ASTNS::Expr>>> args() {
            std::vector<std::unique_ptr<ASTNS::Expr>> res;

            do {
                TRY(arg, std::vector<std::unique_ptr<ASTNS::Expr>>, expr(Precedence::NONE));
                res.push_back(std::move(arg));
            } while (consume_if<TokenType::Comma>());

            return res;
        }
        // primary {{{3
        Maybe<std::unique_ptr<ASTNS::Expr>> primary_expr() {
            auto &prev = peek();
            consume();

            switch (prev.value.type()) {
#define A(a, b) \
    case TokenType::a: \
        return std::make_unique<ASTNS::b>(prev.span, Located<Token>(prev, prev.value.clone()));
                A(BoolLit, BoolLit)
                A(FloatLit, FloatLit)
                A(IntLit, IntLit)
                A(CharLit, CharLit)
                A(StringLit, StringLit)
                A(This, ThisExpr)
#undef A

                case TokenType::OParen: {
                    TRY(e, std::unique_ptr<ASTNS::Expr>, expr(Precedence::NONE));
                    TRY(close, std::unique_ptr<ASTNS::Expr>, expect<TokenType::CParen>("')'"));
                    return std::move(e);
               }

                default:
                    report_abort_noh("invalid primary expr");
            }
        }
        // path {{{3
        Maybe<std::unique_ptr<ASTNS::Expr>> path_expr() {
            TRY(path, std::unique_ptr<ASTNS::Expr>, path());
            return std::make_unique<ASTNS::PathExpr>(path->span(), std::move(path));
        }
        // paths {{{2
        Maybe<std::unique_ptr<ASTNS::Path>> path() {
            std::vector<Located<Token>> segments;
            do {
                TRY(seg, std::unique_ptr<ASTNS::Path>, expect<TokenType::Identifier>("path segment"))
                segments.push_back(Located<Token>(seg, seg.value.clone()));
            } while (consume_if<TokenType::DoubleColon>());

            return std::make_unique<ASTNS::Path>(span_from_vec(segments, segments[0].span), std::move(segments));
        }
        // fields {{{2
        Lexer &lexer;
        File &source;

        bool errored;

        Maybe<Located<Token>> prev_token;
        Located<Token> next_token;
        // helpers {{{2
        // sync {{{3
        void synchronize(std::function<bool(Maybe<Located<Token>> const &, Located<Token> const &)> pred) {
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
            return peek().value.is<TokenType::_EOF>();
        }
        // peek, prev {{{3
        Located<Token> &peek() {
            return next_token;
        }
        Maybe<Located<Token>> &prev() {
            return prev_token;
        }
        // consume, consume_if, expect, assert_expect {{{3
        void consume() {
            prev_token = std::move(next_token);

            bool lastboom = false;
            while (true) {
                Located<Token> cur (lexer.next_token());

                // TODO: janky code should become less janky
                if (cur.value.is<TokenType::Error>()) {
                    errored = true;
                    cur.value.as<TokenType::Error>().err->report();
                } else if (lastboom && cur.value.is<TokenType::Newline>())
                    ;
                else if (cur.value.is<TokenType::Boom>())
                    lastboom = true;
                else {
                    lastboom = false;
                    next_token = std::move(cur);
                    return;
                }
            }
        }

        template <TokenType TokenType>
        bool consume_if() {
            if (peek().value.type() == TokenType) {
                consume();
                return true;
            } else {
                return false;
            }
        }

        template <TokenType TokenType>
        Maybe<Located<Token> const &> expect(std::string const &what) {
            if (peek().value.type() == TokenType) {
                auto &tok = peek();
                consume();

                return tok;
            } else {
                Errors::Expected(peek().span, what).report();
                return Maybe<Located<Token> const &>();
            }
        }

        template <TokenType TokenType>
        Located<Token> const &assert_expect() {
            if (peek().value.type() == TokenType) {
                auto &tok = peek();
                consume();

                return tok;
            } else {
                report_abort_noh("assert_expect failed");
            }
        }
        // span {{{3
        template <typename T>
        Span span_from_vec(std::vector<std::unique_ptr<T>> const &vec, Span const &fallback) {
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

            return start.has() && end.has()
                ? Span(start.get(), end.get())
                : fallback;
        }
        template <typename T>
        Span span_from_vec(std::vector<Located<T>> const &vec, Span const &fallback) {
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

            return start.has() && end.has()
                ? Span(start.get(), end.get())
                : fallback;
        }
        Span join_span(Span const &l, Span const &r) {
            return Span(l.start, r.end);
        }
        // thing list {{{3
        template <typename Ret>
        std::vector<Ret> thing_list_no_separator(TokenPredicate stop, std::function<bool(Maybe<Located<Token>> const &, Located<Token> const &)> sync_pred, std::function<Maybe<Ret>(Parser *)> thing_fun) {
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
