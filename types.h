// -*- C++ -*-
#ifndef CMP_TYPES_H
#define CMP_TYPES_H

#include <cassert>
#include <cstdlib>
#include <cstring>
#include <optional>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>

namespace cmp {

struct Type;
struct Decl;
struct VarDecl;
struct StructDecl;
struct EnumDecl;
struct FuncDecl;
struct Sema;
struct VarDecl;
struct StructDecl;
struct EnumDecl;
struct FuncDecl;

// 'Name' corresponds to a single unique identifier string in the source text.
// There may be multiple occurrences of a string in the source text, but only
// one instance of the matching Name can reside in the name table.
struct Name {
    const char *text;
};

// 'NameTable' is a hash table of Names queried by their string value.  It
// serves to reduce the number of string hashing operation, since we can look
// up the symbol table using Name instead of raw char * throughout the semantic
// analysis.
struct NameTable {
    Name *push(const char *s) {
        return pushlen(s, strlen(s));
    }
    Name *pushlen(const char *s, size_t len) {
        Name *found = get(std::string{s, len});
        if (found)
            return found;

        Name n{strndup(s, len)};
        auto pair = map.insert({std::string{s, len}, n});
        return &pair.first->second;
    }
    Name *get(const std::string &s) {
        auto found = map.find(s);
        if (found == map.end()) {
            return nullptr;
        } else {
            return &found->second;
        }
    }
    ~NameTable() {
        for (auto &m : map) {
            free((void *)m.second.text);
        }
    }
    std::unordered_map<std::string, Name> map;
};

enum class TypeKind {
    value, // built-in, struct
    ptr,
    ref,
    var_ref,
};

// 'Type' represents a type, whether it be a built-in type, a user-defined
// struct, or a reference to another of those.  Similar to Names, Types are
// designed to exist as singular instances in the lifetime of the compiler, and
// are meant to be compared by a simple pointer comparison.
//
// Types should be allocated on the heap and stored as a pointer member in AST
// nodes, rather than stored as a by-value member, because their presence may
// outlive the lexical scope of a single AST node. TODO: say about whether
// storing them in memory pools or the scoped table.
struct Type {
    TypeKind kind = TypeKind::value;
    // Name of the type. TODO: include & or [] in the name?
    Name *name = nullptr;
    // Whether this is a builtin type or not.
    bool builtin = false;
    // True if this type is copyable, e.g. can be used in the RHS of a regular
    // assignment statement (=). Precise value will be determined in the type
    // checking phase.
    bool copyable = true;
    union {
        // Back-reference to the decl object that defines this value type.
        Decl *type_decl = nullptr;
        // The target type that this type refers to.  If this is a non-reference
        // or a non-pointer type, the value should be null.
        Type *referee_type;
    };

    // Built-in value types.
    Type(Name *n) : kind(TypeKind::value), name(n), builtin(true) {}
    // Struct types.
    Type(TypeKind k, Name *n, Decl *td) : kind(k), name(n), type_decl(td) {}
    // Reference types.
    // TODO: copyable?
    Type(Name *n, TypeKind ptr_kind, Type *referee_type);

    std::string str() const { return name->text; }

    // Returns true if this type is a builtin type.
    bool is_builtin(Sema &sema) const;

    // Returns true if this type is an enum.
    bool isEnum() const;

    StructDecl *getStructDecl();
    EnumDecl *getEnumDecl();
};

} // namespace cmp

#endif
