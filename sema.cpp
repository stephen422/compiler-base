#include "sema.h"
#include <iostream>

namespace cmp {

void Type::print() const {
    if (name) {
        std::cout << name->text;
    }
}

void Declaration::print() const {
    std::cout << name->text << ":" << type.name->text;
}

void Semantics::error(size_t pos, const std::string &msg) {
    auto loc = src.locate(pos);
    std::cerr << loc.filename << ":" << loc.line << ":" << loc.col << ": ";
    std::cerr << "error: " << msg << std::endl;
    exit(1);
}

static void initialize_builtin_types(Semantics &sema) {
    Name *int_name = sema.name_table.find("int");
    Type int_type{int_name};
    sema.int_type = sema.type_table.insert({int_name, int_type});
    Name *i64_name = sema.name_table.find("i64");
    Type i64_type{int_name};
    sema.i64_type = sema.type_table.insert({i64_name, i64_type});
}

void semantic_analyze(Semantics &sema, Ast &ast) {
    initialize_builtin_types(sema);
    ast.root->traverse(sema);
}

} // namespace cmp
