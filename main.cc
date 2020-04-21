#include "sema.h"
#include "parser.h"
#include "lexer.h"

using namespace cmp;

struct Driver {
  const std::string filename;
  std::vector<Error> errors;
  std::vector<Error> beacons;

  bool compile_from_path(const Path &path);
  bool no_errors() const { return errors.empty(); }
};

bool Driver::compile_from_path(const Path &path) {
  fmt::print("Parse:\n");
  Source src{path};
  Lexer lexer{src};
  Parser parser{lexer, errors, beacons};
  auto ast = parser.parse();
  if (!no_errors())
    return false;

  Sema sema{parser};
  setup_builtin_types(sema);
  fmt::print("Name binding:\n");
  NameBinder nb{sema};
  nb.visitFile(static_cast<File *>(ast.root));
  if (!no_errors())
    return false;

  fmt::print("Type checking:\n");
  TypeChecker tc{sema};
  tc.visitFile(static_cast<File *>(ast.root));
  if (!no_errors())
    return false;

  fmt::print("Return checking:\n");
  ReturnChecker rc{sema};
  rc.visitFile(static_cast<File *>(ast.root), nullptr);
  if (!no_errors())
    return false;

  return true;
}

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

    Driver driver;
    driver.compile_from_path(Path{"../test_parser.txt"});
    Driver driver2;
    driver2.compile_from_path(Path{"../test_typeck.txt"});

    return EXIT_SUCCESS;
}
