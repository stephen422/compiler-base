// -*- C++ -*-
#ifndef SOURCE_H
#define SOURCE_H

#include <iostream>
#include <string>
#include <vector>

/// Source content handler for file reading, position reporting and so
/// on.
class Source {
public:
    // Create from a filepath.
    Source(const std::string &p);

    // Find line and column number of this character in the source text.
    // Both are zero-based indices.
    std::pair<int, int> locate(size_t pos) const;

    const std::string path;
    std::vector<char> buf;
    std::vector<size_t> line_off;
};

#endif
