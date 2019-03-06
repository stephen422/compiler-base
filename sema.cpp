#include "sema.h"
#include "source.h"
#include "ast.h"
#include <iostream>
#include <cassert>

namespace cmp {

std::string Type::to_string() const {
    return name->text;
}

std::string Declaration::to_string() const {
    return name->text + ":" + type.to_string();
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
    Type i64_type{i64_name};
    sema.i64_type = sema.type_table.insert({i64_name, i64_type});
}

// @Future: inefficient string operations?
Type *get_reference_type(Semantics &sema, Type *type) {
    Name *name = sema.name_table.find_or_insert("&" + type->name->text);
    Type ref_type {name, type, true};
    if (auto found = sema.type_table.find(name)) {
        return found;
    }
    return sema.type_table.insert({name, ref_type});
}

void semantic_analyze(Semantics &sema, Ast &ast) {
    initialize_builtin_types(sema);
    ast.root->traverse(sema);
}

} // namespace cmp
