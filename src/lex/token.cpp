#include <variant>
#include <type_traits>

#include "utils/assert.h"

#define KEEP_TOKEN_TYPES_MACRO
#include "lex/token.h"

// this is an extremely messy source file because macros and template instantiations
// it also compiles extremely slowly because of all the template instantiations and enumerating token types

class Token::TokenImpl {
public:
#define TOKENIMPL_CONSTRUCTOR(ttname) \
    TokenImpl(TokenTypes::ttname tok): data(std::move(tok)) {}
    TOKEN_TYPES(TOKENIMPL_CONSTRUCTOR)
#undef TOKENIMPL_CONSTRUCTOR

    std::variant<
#define VARIANT_ARG(name) TokenTypes::name,
        TOKEN_TYPES(VARIANT_ARG)
#undef VARIANT_ARG
        int // because trailing comma
    > data;
};

#define TOKEN_CONSTRUCTOR(ttname) \
    Token::Token(TokenTypes::ttname tok): pimpl(std::make_unique<TokenImpl>(std::move(tok))) {}
TOKEN_TYPES(TOKEN_CONSTRUCTOR)
#undef TOKEN_CONSTRUCTOR

// defaulted destructor, move constructor, and move assignment operator
Token::~Token() = default;
Token::Token(Token &&) = default;
Token &Token::operator=(Token &&) = default;

template <TokenType ty>
bool Token::is() const {
    return std::holds_alternative<token_detail::TypeOfTy<ty>>(pimpl->data);
}

#define EXPLICIT_INSTANTIATION(name) \
    template Token::is<TokenTypes::name> const (); \
    template Token::as<TokenTypes::name> const ();
#undef EXPLICIT_INSTANTIATION

template <TokenType ty>
token_detail::TypeOfTy<ty> const &Token::as() const {
    ASSERT(is<ty>());
    return std::get<token_detail::TypeOfTy<ty>>(pimpl->data);
}

template <typename TokenType>
struct EnumFromTty;
#define ENUM_FROM_TTY(name) template <> struct EnumFromTty<TokenTypes::name> { static constexpr TokenType value = TokenType::name; };
TOKEN_TYPES(ENUM_FROM_TTY)
#undef ENUM_FROM_TTY

TokenType Token::type() const {
    return std::visit([] (auto &&arg) -> TokenType {
            using T = std::decay_t<decltype(arg)>;
            if constexpr (std::is_same_v<T, int>)
                // push error to runtime so that it compiles
                report_abort_noh("unreachable");
            else
                return EnumFromTty<T>::value;
        }, pimpl->data);
}

template <typename TT>
TT clone_ti(TT const &tt) {
    return tt;
}

TokenTypes::Error clone_ti(TokenTypes::Error const &) {
    // should not be used, the parser should report them immediately, and they shouldnt appear anywhere past the parser
    report_abort_noh("clone error token");
}

Token Token::clone() const {
    return std::visit([] (auto &&arg) -> Token {
            using ArgTT = std::decay_t<decltype(arg)>;
            if constexpr (std::is_same_v<ArgTT, int>)
                report_abort_noh("unreachable");
            else
                return Token(clone_ti(arg));
        }, pimpl->data);
}

template <typename TokenType> struct TokTyStringified;
template <> struct TokTyStringified<TokenTypes::OParen> {
    static constexpr char const *value = "'('";
};
template <> struct TokTyStringified<TokenTypes::CParen> {
    static constexpr char const *value = "')'";
};
template <> struct TokTyStringified<TokenTypes::OBrack> {
    static constexpr char const *value = "'['";
};
template <> struct TokTyStringified<TokenTypes::CBrack> {
    static constexpr char const *value = "']'";
};
template <> struct TokTyStringified<TokenTypes::Comma> {
    static constexpr char const *value = "','";
};
template <> struct TokTyStringified<TokenTypes::Period> {
    static constexpr char const *value = "'.'";
};
template <> struct TokTyStringified<TokenTypes::Question> {
    static constexpr char const *value = "'?'";
};
template <> struct TokTyStringified<TokenTypes::Colon> {
    static constexpr char const *value = "':'";
};
template <> struct TokTyStringified<TokenTypes::Bang> {
    static constexpr char const *value = "'!'";
};
template <> struct TokTyStringified<TokenTypes::Plus> {
    static constexpr char const *value = "'+'";
};
template <> struct TokTyStringified<TokenTypes::Minus> {
    static constexpr char const *value = "'-'";
};
template <> struct TokTyStringified<TokenTypes::Star> {
    static constexpr char const *value = "'*'";
};
template <> struct TokTyStringified<TokenTypes::Slash> {
    static constexpr char const *value = "'/'";
};
template <> struct TokTyStringified<TokenTypes::Percent> {
    static constexpr char const *value = "'%'";
};
template <> struct TokTyStringified<TokenTypes::Equal> {
    static constexpr char const *value = "'='";
};
template <> struct TokTyStringified<TokenTypes::Greater> {
    static constexpr char const *value = "'>'";
};
template <> struct TokTyStringified<TokenTypes::Less> {
    static constexpr char const *value = "'<'";
};
template <> struct TokTyStringified<TokenTypes::Tilde> {
    static constexpr char const *value = "'~'";
};
template <> struct TokTyStringified<TokenTypes::Amper> {
    static constexpr char const *value = "'&'";
};
template <> struct TokTyStringified<TokenTypes::Pipe> {
    static constexpr char const *value = "'|'";
};
template <> struct TokTyStringified<TokenTypes::Caret> {
    static constexpr char const *value = "'^'";
};
template <> struct TokTyStringified<TokenTypes::Dollar> {
    static constexpr char const *value = "'$'";
};
template <> struct TokTyStringified<TokenTypes::Hash> {
    static constexpr char const *value = "'#'";
};
template <> struct TokTyStringified<TokenTypes::RightArrow> {
    static constexpr char const *value = "'->'";
};
template <> struct TokTyStringified<TokenTypes::LeftArrow> {
    static constexpr char const *value = "'<-'";
};
template <> struct TokTyStringified<TokenTypes::DoublePlus> {
    static constexpr char const *value = "'++'";
};
template <> struct TokTyStringified<TokenTypes::DoubleMinus> {
    static constexpr char const *value = "'--'";
};
template <> struct TokTyStringified<TokenTypes::DoubleGreater> {
    static constexpr char const *value = "'>>'";
};
template <> struct TokTyStringified<TokenTypes::DoubleLess> {
    static constexpr char const *value = "'<<'";
};
template <> struct TokTyStringified<TokenTypes::DoubleAmper> {
    static constexpr char const *value = "'&&'";
};
template <> struct TokTyStringified<TokenTypes::DoublePipe> {
    static constexpr char const *value = "'||'";
};
template <> struct TokTyStringified<TokenTypes::DoubleEqual> {
    static constexpr char const *value = "'=='";
};
template <> struct TokTyStringified<TokenTypes::DoubleColon> {
    static constexpr char const *value = "'::'";
};
template <> struct TokTyStringified<TokenTypes::PlusEqual> {
    static constexpr char const *value = "'+='";
};
template <> struct TokTyStringified<TokenTypes::MinusEqual> {
    static constexpr char const *value = "'-='";
};
template <> struct TokTyStringified<TokenTypes::StarEqual> {
    static constexpr char const *value = "'*='";
};
template <> struct TokTyStringified<TokenTypes::SlashEqual> {
    static constexpr char const *value = "'/='";
};
template <> struct TokTyStringified<TokenTypes::BangEqual> {
    static constexpr char const *value = "'!='";
};
template <> struct TokTyStringified<TokenTypes::GreaterEqual> {
    static constexpr char const *value = "'>='";
};
template <> struct TokTyStringified<TokenTypes::LessEqual> {
    static constexpr char const *value = "'<='";
};
template <> struct TokTyStringified<TokenTypes::PercentEqual> {
    static constexpr char const *value = "'%='";
};
template <> struct TokTyStringified<TokenTypes::DoubleLessEqual> {
    static constexpr char const *value = "'<<='";
};
template <> struct TokTyStringified<TokenTypes::DoubleGreaterEqual> {
    static constexpr char const *value = "'>>='";
};
template <> struct TokTyStringified<TokenTypes::AmperEqual> {
    static constexpr char const *value = "'&='";
};
template <> struct TokTyStringified<TokenTypes::PipeEqual> {
    static constexpr char const *value = "'|='";
};
template <> struct TokTyStringified<TokenTypes::CaretEqual> {
    static constexpr char const *value = "'^='";
};
template <> struct TokTyStringified<TokenTypes::Identifier> {
    static constexpr char const *value = "identifier";
};
template <> struct TokTyStringified<TokenTypes::CharLit> {
    static constexpr char const *value = "char literal";
};
template <> struct TokTyStringified<TokenTypes::StringLit> {
    static constexpr char const *value = "string literal";
};
template <> struct TokTyStringified<TokenTypes::IntLit> {
    static constexpr char const *value = "integer literal";
};
template <> struct TokTyStringified<TokenTypes::FloatLit> {
    static constexpr char const *value = "floating point literal";
};
template <> struct TokTyStringified<TokenTypes::BoolLit> {
    static constexpr char const *value = "bool literal";
};
template <> struct TokTyStringified<TokenTypes::This> {
    static constexpr char const *value = "'this'";
};
template <> struct TokTyStringified<TokenTypes::Var> {
    static constexpr char const *value = "'var'";
};
template <> struct TokTyStringified<TokenTypes::Fun> {
    static constexpr char const *value = "'fun'";
};
template <> struct TokTyStringified<TokenTypes::Let> {
    static constexpr char const *value = "'let'";
};
template <> struct TokTyStringified<TokenTypes::Mut> {
    static constexpr char const *value = "'mut'";
};
template <> struct TokTyStringified<TokenTypes::Data> {
    static constexpr char const *value = "'data'";
};
template <> struct TokTyStringified<TokenTypes::Impl> {
    static constexpr char const *value = "'impl'";
};
template <> struct TokTyStringified<TokenTypes::Return> {
    static constexpr char const *value = "'return'";
};
template <> struct TokTyStringified<TokenTypes::While> {
    static constexpr char const *value = "'while'";
};
template <> struct TokTyStringified<TokenTypes::For> {
    static constexpr char const *value = "'for'";
};
template <> struct TokTyStringified<TokenTypes::If> {
    static constexpr char const *value = "'if'";
};
template <> struct TokTyStringified<TokenTypes::Else> {
    static constexpr char const *value = "'else'";
};
template <> struct TokTyStringified<TokenTypes::Case> {
    static constexpr char const *value = "'case'";
};
template <> struct TokTyStringified<TokenTypes::Break> {
    static constexpr char const *value = "'break'";
};
template <> struct TokTyStringified<TokenTypes::Continue> {
    static constexpr char const *value = "'continue'";
};
template <> struct TokTyStringified<TokenTypes::Boom> {
    static constexpr char const *value = "boom";
};
template <> struct TokTyStringified<TokenTypes::OBrace> {
    static constexpr char const *value = "'{'";
};
template <> struct TokTyStringified<TokenTypes::CBrace> {
    static constexpr char const *value = "'}'";
};
template <> struct TokTyStringified<TokenTypes::Semicolon> {
    static constexpr char const *value = "';'";
};
template <> struct TokTyStringified<TokenTypes::Indent> {
    static constexpr char const *value = "indent";
};
template <> struct TokTyStringified<TokenTypes::Dedent> {
    static constexpr char const *value = "dedent";
};
template <> struct TokTyStringified<TokenTypes::Newline> {
    static constexpr char const *value = "newline";
};
template <> struct TokTyStringified<TokenTypes::_EOF> {
    static constexpr char const *value = "end of file";
};
template <> struct TokTyStringified<TokenTypes::Error> {
    static constexpr char const *value = "syntax error";
};

std::string Token::stringify_type() const {
    return std::visit([] (auto &&arg) -> std::string {
            using T = std::decay_t<decltype(arg)>;
            if constexpr (std::is_same_v<T, int>)
                report_abort_noh("unreachable");
            else
                return TokTyStringified<T>::value;
        }, pimpl->data);
}

