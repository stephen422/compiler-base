#include "driver.h"

using namespace cmp;

int main(int argc, char **argv) {
  if (argc < 2) {
    fprintf(stderr, "error: no filename specified\n");
    return 1;
  }

  // XXX: We don't even need to declare Driver variables, why not make these
  // free functions?
  auto d1 = Driver::from_path(Path{argv[1]});
  if (!d1.compile()) {
      return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
