#include "Source.h"
#include <fstream>
#include <iostream>
#include <sstream>
#include <vector>

void use_stringstream(std::ifstream &in) {
  std::stringstream sstr;
  sstr << in.rdbuf();
}

void use_read(std::ifstream &in) {
  // This is ~2x faster than the 'stringstream << buf.rdbuf()' method
  // (https://stackoverflow.com/questions/18816126/c-read-the-whole-file-in-buffer).
  // Might try to rewrite this to use code-level buffering, but doubt
  // that will ever be necessary...
  in.seekg(0, std::ios::end);
  auto size = in.tellg();
  std::cout << "size: " << size << std::endl;
  in.seekg(0, std::ios::beg);

  std::vector<char> buffer(size);
  in.read(buffer.data(), size);
}
