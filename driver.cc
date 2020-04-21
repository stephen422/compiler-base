#include "driver.h"
#include "parser.h"
#include "sema.h"

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

