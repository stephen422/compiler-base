// -*- C++ -*-
#ifndef TYPES_H
#define TYPES_H

#include <cassert>
#include <optional>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>

namespace cmp {

struct Type;
struct Sema;

// 'Name' corresponds to a single unique identifier string in the source text.
// There may be multiple occurrences of a string in the source text, but only
// one instance of the matching Name can reside in the name table.
struct Name {
  std::string text;

  const char *str() const { return text.c_str(); }
};

// 'NameTable' is a hash table of Names queried by their string value.  It
// serves to reduce the number of string hashing operation, since we can look
// up the symbol table using Name instead of raw char * throughout the semantic
// analysis.
struct NameTable {
  Name *get_or_add(const std::string &s) {
    Name *found = get(s);
    if (found) {
      return found;
    }
    auto pair = map.insert({s, {s}});
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
  std::unordered_map<std::string, Name> map;
};

// Declaration of a variable. Includes struct field variables.
struct VarDecl {
    Name *name = nullptr;
    Type *type = nullptr;
    // Decl of the var that this var references to.  Used for borrow checking.
    VarDecl *borrowee = nullptr;

    VarDecl(Name *n) : name(n) {}
};

// Declaration of a type, e.g. struct or enum.
struct StructDecl {
    Name *name = nullptr;
    Type *type = nullptr;
    std::vector<VarDecl *> fields;

    StructDecl(Name *n) : name(n) {}
};

// Declaration of an enum.
struct EnumDecl {
    Name *name = nullptr;
    Type *type = nullptr;
    std::vector<StructDecl *> variants;

    EnumDecl(Name *n) : name(n) {}
};

// Declaration of a function.
struct FuncDecl {
    Name *name = nullptr;
    Type *ret_ty = nullptr;
    std::vector<VarDecl *> args;

    FuncDecl(Name *n) : name(n) {}
    size_t args_count() const { return args.size(); }
    // XXX: might false-report before typecheck is completed
    bool is_void(Sema &sema) const;
};

// 'Decl' represents declaration of a variable, a function, or a type.
// All Decls are stored in a global pool.  Scoped Decl tables act on the
// references of these pooled Decls to determine undeclared-use or
// redefinition.
// TODO: Clarify the definition. Should type names have a Decl too?  What is
// the 'type' member of a StructDecl?
using Decl = std::variant<VarDecl *, StructDecl *, EnumDecl *, FuncDecl *>;
using DeclMemBlock = std::variant<VarDecl, StructDecl, EnumDecl, FuncDecl>;

template <typename T> bool decl_is(const Decl decl) {
    return std::holds_alternative<T *>(decl);
}
template <typename T> T *decl_as(const Decl decl) {
    if (std::holds_alternative<T *>(decl))
        return std::get<T *>(decl);
    else
        return nullptr;
}
std::optional<Type *> decl_get_type(const Decl decl);

// 'Type' represents a type, whether it be built-in, user-defined, or a
// reference to another type.  It exists separately from the AST node TypeExpr.
// Similar to Names, Types are designed to be comparable by simply comparing
// the value of raw Type pointers.
//
// TODO: switch to union/variant?
enum class TypeKind {
    value, // built-in, struct
    ref,
    array,
};

// Elementary types, stripped of informations such as reference and mutability.
struct TypeBase {
    Name *name = nullptr;
    // Back-reference to the Decl object that defines this type.
    // XXX: not used?
    // Decl decl;
};

struct Type {
    TypeKind kind;
    // Name of the type. TODO: include & or [] in the name?
    Name *name = nullptr;
    // Is this a builtin type?
    bool builtin = false;
    // True if this type is copyable, e.g. can be used in the RHS of a regular
    // assignment statement (=). Precise value will be determined in the type
    // checking phase.
    bool copyable = true;
    union {
        // Back-reference to the decl object that defines this value type.
        Decl type_decl{};
        // The type that this type refers to.  If it is a non-reference type,
        // this should be null.
        Type *base_type;
    };

    Type(Name *n) : kind(TypeKind::value), name(n), builtin(true) {}
    // TODO: copyable?
    Type(TypeKind k, Name *n, Type *bt)
        : kind(k), name(n), copyable(true), base_type(bt) {}
    Type(TypeKind k, Name *n, Decl td) : kind(k), name(n), type_decl(td) {}

    const char *str() const;

    bool is_struct() const {
        // TODO: should base_type be null too?
        return kind == TypeKind::value &&
               std::holds_alternative<StructDecl *>(type_decl);
    }
    bool is_enum() const {
        // TODO: should base_type be null too?
        return kind == TypeKind::value &&
               std::holds_alternative<EnumDecl *>(type_decl);
    }
    bool is_member_accessible() const { return is_struct() || is_enum(); }

    StructDecl *get_struct_decl() {
        assert(kind == TypeKind::value);
        assert(std::holds_alternative<StructDecl *>(type_decl));
        return std::get<StructDecl *>(type_decl);
    }
    EnumDecl *get_enum_decl() {
        assert(kind == TypeKind::value);
        assert(std::holds_alternative<EnumDecl *>(type_decl));
        return std::get<EnumDecl *>(type_decl);
    }
};

} // namespace cmp

#endif
