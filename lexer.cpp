#include "lexer.h"
#include <cctype>

std::ostream& operator<<(std::ostream& os, const Token& token)
{
    os << "[" << token.lit << "]";
    return os;
}

// Side note: upon quick testing, std::find families are as fast as or even
// faster than C loops, thanks to loop unrolling.

Token Lexer::lex_ident()
{
    auto isalpha = [](char c) { return std::isalnum(c) || c == '_'; };
    look = std::find_if_not(curr, eos(), isalpha);
    std::string_view lit{curr, static_cast<size_t>(look - curr)};

    // keyword lookup
    for (auto &[lit, type] : keyword_map) {
        std::string_view sv{curr, lit.length()};
        if (sv == lit)
            return make_token_with_literal(type);
    }
    // match fail
    return make_token_with_literal(TokenType::ident);
}

Token Lexer::lex_number()
{
    look = std::find_if_not(curr, eos(), isdigit);
    return make_token_with_literal(TokenType::number);
}

Token Lexer::lex_string()
{
    look = curr + 1; // skip "
    while (look != eos()) {
        look = std::find_if(curr, eos(),
                            [](char c) { return ((c == '\\') || (c == '"')); });
        if (*look == '"') {
            // skip " and end
            look++;
            break;
        } else {
            // skip the escaped character '\x'
            look += 2;
        }
    }
    return make_token_with_literal(TokenType::string);
}

Token Lexer::lex_comment()
{
    look = std::find(curr, eos(), '\n');
    return make_token_with_literal(TokenType::comment);
}

Token Lexer::lex_symbol()
{
    for (auto &[type, lit] : symbol_map) {
        std::string_view sv{curr, lit.length()};
        if (sv == lit) {
            look = curr + lit.length();
            return make_token_with_literal(type);
        }
    }
    // match fail
    look++;
    return make_token(TokenType::none);
}

Token Lexer::make_token(TokenType type)
{
    return Token{type, pos()};
}

Token Lexer::make_token_with_literal(TokenType type)
{
    std::string_view lit{curr, static_cast<size_t>(look - curr)};
    return Token{type, pos(), lit};
}

Token Lexer::lex()
{
    Token tok;
    skip_whitespace();

    if (curr == eos())
        return Token{TokenType::eos, pos()};

    switch (*curr) {
    case 0:
        // TODO
        std::cerr << "unexpected null in source\n";
        break;
    case '"':
        tok = lex_string();
        break;
    default:
        if (std::isalpha(*curr) || *curr == '_') {
            tok = lex_ident();
        } else if (std::isdigit(*curr)) {
            tok = lex_number();
        } else {
            tok = lex_symbol();
        }
        break;
    }

    /*
    } else if (*curr == '/') {
        // Slashes may be either divides or comments
        if (curr + 1 != eos() && *(curr + 1) == '/') {
            auto tok = lex_comment();
            return tok;
        }
    }
    */

    // Advance lexed position
    curr = look;

    return tok;
}

Token Lexer::peek()
{
    auto save = curr;
    auto token = lex();
    curr = save;
    return token;
}

void Lexer::skip_whitespace()
{
    curr = std::find_if_not(curr, eos(), isspace);
}
