#include <vector>

/// Source content handler for file reading, position reporting and so
/// on.
class Source {
  Source() {}

private:
  // The internal buffer that contains the content of the file.
  std::vector<char> buffer;
};
