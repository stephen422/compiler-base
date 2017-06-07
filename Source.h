// -*- C++ -*-

#include <iostream>
#include <string>
#include <vector>

/// Source content handler for file reading, position reporting and so
/// on.
class Source {
public:
  const std::string path;
  Source(const std::string &path) : path(path), read(false) {}

  const std::vector<char> &buffer();

private:
  // The internal buffer that contains the content of the file.
  std::vector<char> buf;

  // Indicates whether the file is fully read.
  bool read;
};
