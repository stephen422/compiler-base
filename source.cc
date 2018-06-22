#include "source.hh"
#include <cstring>
#include <fstream>
#include <sstream>
#include <vector>

void use_stringstream(std::ifstream& in) {
  std::stringstream sstr;
  sstr << in.rdbuf();
}

Source::Source(const std::string &p) : path(p) {
  std::ifstream in{path, std::ios::binary};
  if (!in) {
    std::cerr << strerror(errno) << std::endl;
    exit(EXIT_FAILURE);
  }

  in.seekg(0, std::ios::end);
  auto size = in.tellg();
  in.seekg(0, std::ios::beg);

  buf.resize(size);
  in.read(buf.data(), size);
}
