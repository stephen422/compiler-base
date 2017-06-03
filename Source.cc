#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include "Source.h"

void use_stringstream(std::ifstream& in) {
  std::stringstream sstr;
  sstr << in.rdbuf();
}

void use_read(std::ifstream& in) {
  // This is ~2x faster than the 'stringstream << buf.rdbuf()' method
  // (https://stackoverflow.com/questions/18816126/c-read-the-whole-file-in-buffer).
  // Might try to rewrite this to use code-level buffering, but doubt
  // that will ever be necessary...
  in.seekg(0, std::ios::end);
  auto size = in.tellg();
  in.seekg(0, std::ios::beg);

  std::vector<char> buffer(size);
  in.read(buffer.data(), size);
}

int main(int argc, char **argv) {
  std::ifstream file{argv[1], std::ios::binary};
  use_read(file);
  //use_stringstream(file);
  return 0;
}
