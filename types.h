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
struct VarDecl;
struct StructDecl;
struct EnumDecl;
struct FuncDecl;

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
template <typename T> T *declCast(const Decl decl) {
    if (std::holds_alternative<T *>(decl))
        return std::get<T *>(decl);
    else
        return nullptr;
}
std::optional<Type *> declGetType(const Decl decl);

// Elementary types, stripped of informations such as reference.
// They include built-in value types such as int and bool, and user-declared
// structs and enums.  They are meant to exist as singular instances, and their
// comparison is done by a direct pointer comparison.
struct BaseType {
    Name *name = nullptr;
    // Back-reference to the Decl object that defines this type.
    // XXX: not used?
    // Decl decl;
};

enum class TypeKind {
    value, // built-in, struct
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
//
// TODO: switch to union/variant?
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
    Decl type_decl{};
    // The target type of this reference type.  If this is a non-reference
    // type, the value should be null.
    Type *referee_type;
  };

  // Built-in value types.
  Type(Name *n) : kind(TypeKind::value), name(n), builtin(true) {}
  // Struct types.
  Type(TypeKind k, Name *n, Decl td) : kind(k), name(n), type_decl(td) {}
  // Reference types.
  // TODO: copyable?
  Type(Name *n, bool mut, Type *bt);

  const char *str() const { return name->str(); }

  bool isReferenceType() const {
    return kind == TypeKind::ref || kind == TypeKind::var_ref;
  }
  bool isStruct() const {
    // TODO: should base_type be null too?
    return kind == TypeKind::value &&
           std::holds_alternative<StructDecl *>(type_decl);
  }
  bool isEnum() const {
    // TODO: should base_type be null too?
    return kind == TypeKind::value &&
           std::holds_alternative<EnumDecl *>(type_decl);
  }
  bool isMemberAccessible() const { return isStruct() || isEnum(); }

  StructDecl *getStructDecl() {
    assert(kind == TypeKind::value);
    assert(std::holds_alternative<StructDecl *>(type_decl));
    return std::get<StructDecl *>(type_decl);
  }
  EnumDecl *getEnumDecl() {
    assert(kind == TypeKind::value);
    assert(std::holds_alternative<EnumDecl *>(type_decl));
    return std::get<EnumDecl *>(type_decl);
  }
};

Type *getReferenceType(Sema &sema, bool mut, Type *type);

// Declaration of a variable. Includes struct field variables.
struct VarDecl {
  Name *name = nullptr;
  Type *type = nullptr;
  // Decl of the var that this var references to.  Used for borrow checking.
  VarDecl *borrowee = nullptr;
  bool mut = false;

  VarDecl(Name *n, bool m) : name(n), mut(m) {}
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
    bool isVoid(Sema &sema) const;
};

} // namespace cmp

#endif
