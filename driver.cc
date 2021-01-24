#include "driver.h"
#include "ast.h"
#include "parser.h"
#include "sema.h"

bool Driver::compile() {
    Lexer lexer{source};
    Sema sema{source, errors, beacons};
    Parser parser{lexer, sema};

    auto node = parser.parse();
    if (!no_errors()) {
        return false;
    }

    setup_builtin_types(sema);
    typecheck(sema, node);
    QbeGenerator c{sema, "out.qbe"};
    codegen(c, node);
    fflush(c.file);

    system("$HOME/build/qbe/bin/qbe -o out.s out.qbe");
    system("gcc -o out out.s");

    return true;
}
