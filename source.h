// -*- C++ -*-
#ifndef SOURCE_H
#define SOURCE_H

#include "fmt/core.h"
#include "fmt/format.h"
#include <string>
#include <vector>

namespace cmp {

struct Path {
    std::string path;
};

/// SourceLoc represents a position (line, col) in the source text.
struct SourceLoc {
    std::string filename;
    int line;
    int col;

    std::string str() const;
};

/// Source content handler for file reading, position reporting and so
/// on.
/// TODO: construct from string_view
class Source {
public:
    const std::string filename;
    std::vector<char> buf;
    std::vector<size_t> line_off;

    // Create from a filepath.
    Source(const Path &p);

    // Create source from a string.
    Source(const std::string &text);

    // Return source length.
    size_t length() const { return buf.size(); }

    // Find line and column number of this character in the source text.
    // Both are zero-based indices.
    SourceLoc locate(size_t pos) const;

private:
    // Initialize source text from an istream.
    void init(std::istream &in);
};

} // namespace cmp

#endif
