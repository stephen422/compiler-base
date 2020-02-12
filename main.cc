#include "sema.h"
#include "parser.h"
#include "lexer.h"

using namespace cmp;

void test_lexer(Lexer &lexer) {
    Token token;

    while ((token = lexer.lex()).kind != TokenKind::eos) {
        if (token.kind == TokenKind::none) {
            fmt::print(stderr, "lex error: [{}]: Unrecognized token kind\n", token.toString());
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
    Parser p_parser{src_parser};
    p_parser.parse();
    if (!p_parser.verify())
        return EXIT_FAILURE;

    Source src_sema{Path{"../test_sema.txt"}};
    Parser p_sema{src_sema};
    auto ast = p_sema.parse();
    ast.root->print();
    Sema s{p_sema};
    walkAST(s, ast.root, nameBindPre, nameBindPost);
    // sema(s, ast);
    // if (!s.verify()) {
    //     fmt::print("==== Declaration table ====\n");
    //     s.decl_table.print();
    //     // fmt::print("==== Type table ====\n");
    //     // s.type_table.print();
    // }
#endif

    return EXIT_SUCCESS;
}
