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

    CodeGenerator c{sema, "out.qbe"};
    codegen(c, node);

    return true;
}
