#include "sema.h"
#include <iostream>

namespace cmp {

void Semantics::error(size_t pos, const std::string &msg) {
    auto loc = src.locate(pos);
    std::cerr << loc.filename << ":" << loc.line << ":" << loc.col << ": ";
    std::cerr << "error: " << msg << std::endl;
    exit(1);
}

static void initialize_builtin_types(Semantics &sema) {
    Name *int_name = sema.name_table.find("int");
    Type int_type{int_name};
    sema.type_table.insert({int_name, int_type});
}

void semantic_analyze(Semantics &sema, Ast ast) {
    initialize_builtin_types(sema);
    ast.root->traverse(sema);
}

} // namespace cmp
