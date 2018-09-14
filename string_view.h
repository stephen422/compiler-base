// -*- C++ -*-
#ifndef STRING_VIEW_H
#define STRING_VIEW_H

#include <cstring> // memcmp

/// StringView is a constant reference to a string, or a part of the string.
/// The memory referenced by a StringView is expected to live longer than the
/// StringView.
class StringView {
public:
    using iterator = const char *;

private:
    const char *data;
    size_t len;

public:
    StringView() = default;
    StringView(const char *s, size_t len) : data(s), len(len) {}
    StringView(const char *s) : data(s), len(s ? strlen(s) : 0) {}
    size_t length() const { return len; }
    bool operator==(const StringView &rhs) const {
        return len == rhs.len && std::memcmp(data, rhs.data, len) == 0;
    }
    iterator begin() const { return data; }
    iterator end() const { return data + len; }
    friend std::ostream &operator<<(std::ostream &os, const StringView &sv) {
        os.write(sv.data, sv.len);
        return os;
    }
};

#endif
