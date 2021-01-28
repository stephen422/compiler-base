#include "ast.h"
#include "sema.h"
#include <cassert>
#include <iostream>
#include <sstream>

namespace cmp {

std::string_view AstNode::text(const Source &source) const {
    if (endpos < pos)
        return std::string_view{"(null)"};
    return std::string_view{source.buf.data() + pos, endpos - pos};
}

int AstNode::indent = 0;

// Gets the union of the source ranges of nodes.
std::pair<size_t, size_t>
get_ast_range(std::initializer_list<AstNode *> nodes) {
    size_t min = static_cast<size_t>(-1);
    size_t max = 0; // FIXME not used
    for (auto node : nodes) {
        if (!node)
            continue;
        if (node->pos < min) {
            min = node->pos;
        }
    }
    return {min, max};
}

// Return optional of 'type' member of Decl, or None if this Decl kind doesn't
// have any.
std::optional<Type *> Decl::typemaybe() const {
    if (kind == DeclKind::var) {
        return as<VarDecl>()->type;
    } else if (kind == DeclKind::struct_) {
        return as<StructDecl>()->type;
    } else if (kind == DeclKind::enum_) {
        return as<EnumDecl>()->type;
    } else if (kind == DeclKind::enum_variant) {
        return as<EnumVariantDecl>()->type;
    } else if (kind == DeclKind::func) {
        return {};
    }
    assert(false && "not all decl kinds handled");
}

} // namespace cmp
