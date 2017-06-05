#include "Lexer.h"
#include <cstring>
#include <fstream>
#include <iostream>

extern void use_read(std::ifstream &in);

int main(int argc, char **argv) {
  if (argc < 2) {
    std::cerr << "No filename specified\n";
    return 1;
  }

  std::ifstream file{argv[1], std::ios::binary};
  if (!file) {
    std::cerr << "error: " << strerror(errno) << std::endl;
    return 1;
  }
  use_read(file);
  // use_stringstream(file);
  return 0;
}
