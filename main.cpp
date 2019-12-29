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

    Source src{Path{argv[1]}};
    Lexer lexer{src};

#if 0
    test_lexer(lexer);
#else
    Parser p{lexer};
    auto ast = p.parse();
    ast.root->print();
    p.report();
    p.compareErrors();
    // Semantics sema{src, ast.name_table};
    // fmt::print("After parsing:\n");
    // semantic_analyze(sema, ast);
    // fmt::print("==== Declaration table ====\n");
    // sema.decl_table.print();
    // fmt::print("==== Type table ====\n");
    // sema.type_table.print();
#endif

    return 0;
}
