#include <iostream>
#include <string>
#include <vector>

/// Source content handler for file reading, position reporting and so
/// on.
class Source {
public:
  Source(const std::string_view sv) : buf(cbegin(sv), cend(sv)) {}
  Source(const std::vector<char>& buf) : buf(std::move(buf)) {}

  const std::vector<char>& buffer() const { return buf; }

private:
  const std::string path;
  // The internal buffer that contains the content of the file.
  std::vector<char> buf;
};

Source source_from_file(const std::string& path);
