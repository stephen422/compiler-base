// -*- C++ -*-
#ifndef STRING_VIEW_H
#define STRING_VIEW_H

#include <cstring> // memcmp

/// StringView is a constant reference to a string, or a part of the string.
/// The memory referenced by a StringView is expected to live longer than the
/// StringView.
class StringView {
private:
    const char *ptr;
    size_t len;

public:
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
    friend std::ostream &operator<<(std::ostream &os, const StringView &sv) {
        if (sv.ptr == nullptr) {
            os << "(null)";
        } else {
            os.write(sv.ptr, sv.len);
        }
        return os;
    }
};

#endif
