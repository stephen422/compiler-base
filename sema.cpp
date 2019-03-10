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
    std::cout << "==== Declaration table ====\n";
    decl_table.print();
    std::cout << "==== Type table ====\n";
    type_table.print();
    exit(1);
}

static Type make_type_from_text(Semantics &sema, const std::string &text) {
    Name *name = sema.name_table.find_or_insert(text);
    return Type{name};
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

Semantics::Semantics(Source &src_, NameTable &nt) : src(src_), name_table(nt) {
    Name *int_name = name_table.find_or_insert("int");
    Type int_type{int_name};
    this->int_type = type_table.insert({int_name, int_type});
    Name *i64_name = name_table.find_or_insert("i64");
    Type i64_type{i64_name};
    this->i64_type = type_table.insert({i64_name, i64_type});
}

void semantic_analyze(Semantics &sema, Ast &ast) { ast.root->traverse(sema); }

} // namespace cmp
