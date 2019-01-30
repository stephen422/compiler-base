// -*- C++ -*-
#ifndef SOURCE_H
#define SOURCE_H

#include <iostream>
#include <string>
#include <vector>

class Path {
public:
    Path(const std::string &path) : path(path) {}
    std::string path;
};

/// SourceLoc represents a position (line, col) in the source text.
class SourceLoc {
public:
    std::string filename;
    int line;
    int col;
};

/// Source content handler for file reading, position reporting and so
/// on.
class Source {
public:
    // Create from a filepath.
    Source(const Path &p);

    // Create source from a string.
    Source(const std::string &text);

    // Initialize source text from an istream.
    void init(std::istream &in);

    // Return source length.
    size_t length() const { return buf.size(); }

    // Find line and column number of this character in the source text.
    // Both are zero-based indices.
    SourceLoc locate(size_t pos) const;

    const std::string filename;
    std::vector<char> buf;
    std::vector<size_t> line_off;
};

#endif
