#include "sema.h"
#include "parser.h"
#include "lexer.h"

using namespace cmp;

void test_lexer(Lexer &lexer) {
    Token token;

    while ((token = lexer.lex()).kind != Tok::eos) {
        if (token.kind == Tok::none) {
            fmt::print(stderr, "lex error: [{}]: Unrecognized token kind\n", token.str());
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

    // Source src_sema{Path{"../test_sema.txt"}};
    // Parser p_sema{src_sema};
    // auto ast = p_sema.parse();
    // ast.root->print();
    // Sema s{p_sema};
    // NameBinder n{s};
    // n.visit_file(static_cast<File *>(ast.root));

    Source src_typeck{Path{"../test_typeck.txt"}};
    Parser p_typeck{src_typeck};
    auto ast = p_typeck.parse();
    Sema s_typeck{p_typeck};
    NameBinder n{s_typeck};
    TypeChecker tc{s_typeck};
    n.visit_file(static_cast<File *>(ast.root));
    tc.visit_file(static_cast<File *>(ast.root));

#endif
    return EXIT_SUCCESS;
}
