#include "sema.h"
#include "parser.h"
#include "lexer.h"

using namespace cmp;

void test_lexer(Lexer &lexer) {
    Token token;

    while ((token = lexer.lex()).kind != TokenKind::eos) {
        if (token.kind == TokenKind::none) {
            fmt::print(stderr, "lex error: [{}]: Unrecognized token kind\n", token);
            break;
        }
        fmt::print("[{}], token");
    }
}

int main(int argc, char **argv) {
    if (argc < 2) {
        fmt::print(stderr, "error: no filename specified\n");
        return 1;
    }

#if 0
    test_lexer(lexer);
#else
    Source src_parser{Path{"../test_parser.txt"}};
    Parser p_parser{Lexer{src_parser}};
    if (!p_parser.verify())
        return 1;

    Source src_sema{Path{"../test_sema.txt"}};
    Parser p_sema{Lexer{src_sema}};
    auto ast = p_sema.parse();
    Sema s{p_sema};
    sema(s, ast);
    if (!s.verify()) {
        fmt::print("==== Declaration table ====\n");
        s.decl_table.print();
        // fmt::print("==== Type table ====\n");
        // s.type_table.print();
    }
#endif

    return 0;
}
