// -*- C++ -*-
#ifndef STRING_VIEW_H
#define STRING_VIEW_H

#include "fmt/format.h"
#include <string>
#include <cstring> // memcmp

/// StringView is a constant reference to a string, or a part of the string.
/// The memory referenced by a StringView is expected to live longer than the
/// StringView.
class StringView {
public:
    const char *ptr;
    size_t len;

    using iterator = const char *;

    StringView() = default;
    StringView(const char *s, size_t len) : ptr(s), len(len) {}
    StringView(const char *s) : ptr(s), len(s ? strlen(s) : 0) {}
    const char *data() const { return ptr; }
    size_t length() const { return len; }
    bool operator==(const StringView &rhs) const {
        return len == rhs.len && std::memcmp(ptr, rhs.ptr, len) == 0;
    }
    iterator begin() const { return ptr; }
    iterator end() const { return ptr + len; }
    operator std::string() const { return std::string{ptr, len}; }
    std::string to_string() const { return std::string{ptr, len}; }
};

template <> struct fmt::formatter<StringView> {
    constexpr auto parse(format_parse_context &ctx) { return ctx.begin(); }

    template <typename FormatContext>
    auto format(const StringView &sv, FormatContext &ctx) {
        return format_to(ctx.out(), "{}", sv.to_string());
    }
};

#endif
