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

    return true;
}
