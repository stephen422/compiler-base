#include "driver.h"
#include "ast.h"
#include "parser.h"
#include "sema.h"

bool Driver::compile() {
    Lexer lexer{source};
    Sema sema{source, errors, beacons};
    Parser parser{lexer, sema};

    auto ast = parser.parse();
    if (!no_errors())
        return false;

    setup_builtin_types(sema);
    NameBinding nb{sema};
    nb.visitFile(static_cast<File *>(ast.root));
    if (!no_errors())
        return false;

    TypeChecker tc{sema};
    tc.visitFile(static_cast<File *>(ast.root));
    if (!no_errors())
        return false;

    ReturnChecker rc{sema};
    rc.visitFile(static_cast<File *>(ast.root), nullptr);
    if (!no_errors())
        return false;

    BorrowChecker bc{sema};
    bc.visitFile(static_cast<File *>(ast.root));
    if (!no_errors())
        return false;

    return true;
}

// Report errors to stdout.
void Driver::report() const {
    for (auto e : errors) {
        fmt::print("{}\n", e.str());
    }
}

// See cmp::verify().
bool Driver::verify() { return cmp::verify(source.filename, errors, beacons); }
