#include "Source.hh"
#include <cstring>
#include <fstream>
#include <sstream>
#include <vector>

void use_stringstream(std::ifstream& in) {
  std::stringstream sstr;
  sstr << in.rdbuf();
}

Source source_from_file(const std::string& path) {
  std::ifstream in{path, std::ios::binary};
  if (!in) {
    std::cerr << strerror(errno) << std::endl;
    exit(EXIT_FAILURE);
  }

  // This is ~2x faster than the 'stringstream << buf.rdbuf()' method
  // (https://stackoverflow.com/questions/18816126/c-read-the-whole-file-in-buffer).
  // Might try to rewrite this to use code-level buffering, but I doubt that
  // will ever be necessary...

  in.seekg(0, std::ios::end);
  auto size = in.tellg();
  in.seekg(0, std::ios::beg);

  std::vector<char> buf(size);
  in.read(buf.data(), size);
  buf.resize(in.tellg());
  return Source{std::move(buf)};
}
