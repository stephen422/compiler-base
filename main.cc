#include "sema.h"
#include "parser.h"
#include "lexer.h"

using namespace cmp;

struct Driver {
  const Source source;
  std::vector<Error> errors;
  std::vector<Error> beacons;

  // Construct from a filepath.
  Driver(const Path &path) : source{path} {}
  // Construct from a string text.
  Driver(const std::string &text) : source{text} {};
  static Driver from_path(const Path &path) { return Driver{path}; }
  static Driver from_text(const std::string &text) { return Driver{text}; }

  bool compile();
  void report() const;
  bool verify();
  bool no_errors() const { return errors.empty(); }
};

bool Driver::compile() {
  fmt::print("Parse:\n");
  Lexer lexer{source};
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

// Report errors to stdout.
void Driver::report() const {
  for (auto e : errors)
    fmt::print("{}\n", e.str());
}

// See cmp::verify().
bool Driver::verify() {
    return cmp::verify(source.filename, errors, beacons);
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

    // XXX: We don't even need to declare Driver variables, why not make these
    // free functions?
    auto d1 = Driver::from_path(Path{"../test_parser.txt"});
    d1.compile();
    d1.verify();
    auto d2 = Driver::from_path(Path{"../test_sema.txt"});
    d2.compile();
    d2.verify();
    auto d3 = Driver::from_path(Path{"../test_typeck.txt"});
    d3.compile();
    d3.verify();

    return EXIT_SUCCESS;
}
